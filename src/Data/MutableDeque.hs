{-# LANGUAGE RecordWildCards #-}
module Data.MutableDeque where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableDeque m a = MutableDeque
  { mdVect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    mdHead :: !(MutVar (PrimState m) Int),
    mdTail :: !(MutVar (PrimState m) Int),
    mdMask :: !(MutVar (PrimState m) Int) -- cap - 1
  }

mdNew :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableDeque m a)
mdNew !n = do
  let !_log = if n <= 1 then 1 else finiteBitSize n - countLeadingZeros (n - 1)
      !sz = max 1024 $ shiftL 1 _log
  v <- UM.new sz
  vect <- newMutVar v
  h <- newMutVar 0
  t <- newMutVar 0
  mdMask <- newMutVar (sz - 1)
  return $ MutableDeque vect h t mdMask
{-# INLINE mdNew #-}

mdGrow :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m ()
mdGrow MutableDeque{..} = do
  !v <- readMutVar mdVect
  !h <- readMutVar mdHead
  !t <- readMutVar mdTail
  let !oldSz = UM.length v
      !newSz = oldSz * 2
  newV <- UM.new newSz
  let !lenH = oldSz - h
  UM.unsafeCopy (UM.unsafeTake lenH newV) (UM.unsafeSlice h lenH v)
  UM.unsafeCopy (UM.unsafeSlice lenH t newV) (UM.unsafeTake t v)

  writeMutVar mdVect newV
  writeMutVar mdHead 0
  writeMutVar mdTail oldSz
  writeMutVar mdMask $! newSz - 1
{-# INLINE mdGrow #-}
{-# SPECIALIZE mdGrow :: UM.Unbox a => MutableDeque (ST s) a -> ST s () #-}
{-# SPECIALIZE mdGrow :: UM.Unbox a => MutableDeque IO a -> IO () #-}

mdPushFront :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> a -> m ()
mdPushFront md@MutableDeque{..} !x = do
  !h <- readMutVar mdHead
  !m <- readMutVar mdMask
  let !newH = (h - 1) .&. m
  !t <- readMutVar mdTail
  if newH == t
    then do
      mdGrow md
      mdPushFront md x
    else do
      !v <- readMutVar mdVect
      UM.unsafeWrite v newH x
      writeMutVar mdHead newH
{-# INLINE mdPushFront #-}
{-# SPECIALIZE mdPushFront :: UM.Unbox a => MutableDeque (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE mdPushFront :: UM.Unbox a => MutableDeque IO a -> a -> IO () #-}

mdPushBack :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> a -> m ()
mdPushBack md@MutableDeque{..} !x = do
  !t <- readMutVar mdTail
  !m <- readMutVar mdMask
  let !newT = (t + 1) .&. m
  !h <- readMutVar mdHead
  if newT == h
    then do 
      mdGrow md
      mdPushBack md x
    else do
      !v <- readMutVar mdVect
      UM.unsafeWrite v t x
      writeMutVar mdTail newT
{-# INLINE mdPushBack #-}
{-# SPECIALIZE mdPushBack :: UM.Unbox a => MutableDeque (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE mdPushBack :: UM.Unbox a => MutableDeque IO a -> a -> IO () #-}

mdPopFront :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
mdPopFront MutableDeque{..} = do
  !h <- readMutVar mdHead
  !t <- readMutVar mdTail
  if h == t 
    then return Nothing
    else do
      !v <- readMutVar mdVect
      !m <- readMutVar mdMask
      !top <- UM.unsafeRead v h
      let !newH = (h + 1) .&. m
      writeMutVar mdHead newH
      return $ Just top
{-# INLINE mdPopFront #-}
{-# SPECIALIZE mdPopFront :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE mdPopFront :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

mdPopBack :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
mdPopBack MutableDeque{..} = do
  !h <- readMutVar mdHead
  !t <- readMutVar mdTail
  if h == t 
    then return Nothing
    else do
      !v <- readMutVar mdVect
      !m <- readMutVar mdMask
      let !newT = (t - 1) .&. m
      !bot <- UM.unsafeRead v newT
      writeMutVar mdTail newT
      return $ Just bot
{-# INLINE mdPopBack #-}
{-# SPECIALIZE mdPopBack :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE mdPopBack :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

mdNull :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m Bool
mdNull MutableDeque{..} = (==) <$> readMutVar mdHead <*> readMutVar mdTail
{-# INLINE mdNull #-}
{-# SPECIALIZE mdNull :: UM.Unbox a => MutableDeque (ST s) a -> ST s Bool #-}
{-# SPECIALIZE mdNull :: UM.Unbox a => MutableDeque IO a -> IO Bool #-}

mdTop :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
mdTop MutableDeque{..} = do
  !h <- readMutVar mdHead
  !t <- readMutVar mdTail
  if h == t 
    then return Nothing
    else do
      !v <- readMutVar mdVect
      Just <$> UM.unsafeRead v h
{-# INLINE mdTop #-}
{-# SPECIALIZE mdTop :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE mdTop :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

mdBot :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
mdBot MutableDeque{..} = do
  !h <- readMutVar mdHead
  !t <- readMutVar mdTail
  if h == t 
    then return Nothing
    else do
      !v <- readMutVar mdVect
      !m <- readMutVar mdMask
      let !newT = (t - 1) .&. m
      Just <$> UM.unsafeRead v newT
{-# INLINE mdBot #-}
{-# SPECIALIZE mdBot :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE mdBot :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

mdClear :: PrimMonad m => MutableDeque m a -> m ()
mdClear MutableDeque{..} = do
  writeMutVar mdHead 0
  writeMutVar mdTail 0
{-# INLINE mdClear #-}
{-# SPECIALIZE mdClear :: MutableDeque (ST s) a -> ST s () #-}
{-# SPECIALIZE mdClear :: MutableDeque IO a -> IO () #-}