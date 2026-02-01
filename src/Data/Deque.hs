{-# LANGUAGE RecordWildCards #-}
module Data.Deque where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableDeque m a = MutableDeque
  { vect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    dqHead :: !(MutVar (PrimState m) Int),
    dqTail :: !(MutVar (PrimState m) Int),
    mask :: !(MutVar (PrimState m) Int) -- cap - 1
  }

new :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableDeque m a)
new !n = do
  let !_log = if n <= 1 then 1 else finiteBitSize n - countLeadingZeros (n - 1)
      !sz = max 1024 $ shiftL 1 _log
  v <- UM.new sz
  vect <- newMutVar v
  h <- newMutVar 0
  t <- newMutVar 0
  mask <- newMutVar (sz - 1)
  return $ MutableDeque vect h t mask
{-# INLINE new #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> ST s (MutableDeque (ST s) a) #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> IO (MutableDeque IO a) #-}

grow :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m ()
grow MutableDeque{..} = do
  !v <- readMutVar vect
  !h <- readMutVar dqHead
  !t <- readMutVar dqTail
  let !oldSz = UM.length v
      !newSz = oldSz * 2
  newV <- UM.new newSz
  let !lenH = oldSz - h
  UM.unsafeCopy (UM.unsafeTake lenH newV) (UM.unsafeSlice h lenH v)
  UM.unsafeCopy (UM.unsafeSlice lenH t newV) (UM.unsafeTake t v)

  writeMutVar vect newV
  writeMutVar dqHead 0
  writeMutVar dqTail oldSz
  writeMutVar mask $! newSz - 1
{-# INLINE grow #-}
{-# SPECIALIZE grow :: UM.Unbox a => MutableDeque (ST s) a -> ST s () #-}
{-# SPECIALIZE grow :: UM.Unbox a => MutableDeque IO a -> IO () #-}

pushFront :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> a -> m ()
pushFront md@MutableDeque{..} !x = do
  !h <- readMutVar dqHead
  !m <- readMutVar mask
  let !newH = (h - 1) .&. m
  !t <- readMutVar dqTail
  if newH == t
    then do
      grow md
      pushFront md x
    else do
      !v <- readMutVar vect
      UM.unsafeWrite v newH x
      writeMutVar dqHead newH
{-# INLINE pushFront #-}
{-# SPECIALIZE pushFront :: UM.Unbox a => MutableDeque (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE pushFront :: UM.Unbox a => MutableDeque IO a -> a -> IO () #-}

pushBack :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> a -> m ()
pushBack md@MutableDeque{..} !x = do
  !t <- readMutVar dqTail
  !m <- readMutVar mask
  let !newT = (t + 1) .&. m
  !h <- readMutVar dqHead
  if newT == h
    then do 
      grow md
      pushBack md x
    else do
      !v <- readMutVar vect
      UM.unsafeWrite v t x
      writeMutVar dqTail newT
{-# INLINE pushBack #-}
{-# SPECIALIZE pushBack :: UM.Unbox a => MutableDeque (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE pushBack :: UM.Unbox a => MutableDeque IO a -> a -> IO () #-}

popFront :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
popFront MutableDeque{..} = do
  !h <- readMutVar dqHead
  !t <- readMutVar dqTail
  if h == t then return Nothing
  else do
    !v <- readMutVar vect
    !m <- readMutVar mask
    !top <- UM.unsafeRead v h
    let !newH = (h + 1) .&. m
    writeMutVar dqHead newH
    return $ Just top
{-# INLINE popFront #-}
{-# SPECIALIZE popFront :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE popFront :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

popBack :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m (Maybe a)
popBack MutableDeque{..} = do
  !h <- readMutVar dqHead
  !t <- readMutVar dqTail
  if h == t then return Nothing
  else do
    !v <- readMutVar vect
    !m <- readMutVar mask
    let !newT = (t - 1) .&. m
    !bot <- UM.unsafeRead v newT
    writeMutVar dqTail newT
    return $ Just bot
{-# INLINE popBack #-}
{-# SPECIALIZE popBack :: UM.Unbox a => MutableDeque (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE popBack :: UM.Unbox a => MutableDeque IO a -> IO (Maybe a) #-}

null :: (PrimMonad m, UM.Unbox a) => MutableDeque m a -> m Bool
null MutableDeque{..} = (==) <$> readMutVar dqHead <*> readMutVar dqTail
{-# INLINE null #-}
{-# SPECIALIZE Data.Deque.null :: UM.Unbox a => MutableDeque (ST s) a -> ST s Bool #-}
{-# SPECIALIZE Data.Deque.null :: UM.Unbox a => MutableDeque IO a -> IO Bool #-}