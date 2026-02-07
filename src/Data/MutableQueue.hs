{-# LANGUAGE RecordWildCards #-}
module Data.MutableQueue where
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableQueue m a = MutableQueue
  { mqVect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    mqHead :: !(MutVar (PrimState m) Int),
    mqTail :: !(MutVar (PrimState m) Int)
  }

mqNew :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableQueue m a)
mqNew !sz = do
  v <- UM.new sz
  vect <- newMutVar v
  h <- newMutVar 0
  t <- newMutVar 0
  return $ MutableQueue vect h t

mqPop :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (Maybe a)
mqPop MutableQueue{..} = do
  !v <- readMutVar mqVect
  !h <- readMutVar mqHead
  !t <- readMutVar mqTail
  if h == t 
    then return Nothing
    else do
      !top <- UM.unsafeRead v h
      writeMutVar mqHead $! h + 1
      return $ Just top
{-# INLINE mqPop #-}

mqPush :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> a -> m ()
mqPush MutableQueue{..} !x = do
  !v <- readMutVar mqVect
  !t <- readMutVar mqTail
  !v' <-
    if t < UM.length v 
      then return v
      else do 
        !newV <- UM.unsafeGrow v t
        writeMutVar mqVect newV
        return newV
  UM.unsafeWrite v' t x
  writeMutVar mqTail $! t + 1
{-# INLINABLE mqPush #-}

mqNull :: PrimMonad m => MutableQueue m a -> m Bool
mqNull MutableQueue{..} = (==) <$> readMutVar mqHead <*> readMutVar mqTail
{-# INLINE mqNull #-}

mqTop :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (Maybe a)
mqTop MutableQueue{..} = do
  !v <- readMutVar mqVect
  !h <- readMutVar mqHead
  !t <- readMutVar mqTail
  if h == t 
    then return Nothing
    else Just <$> UM.unsafeRead v h
{-# INLINE mqTop #-}

mqClear :: PrimMonad m => MutableQueue m a -> m ()
mqClear MutableQueue{..} = do
  writeMutVar mqHead 0
  writeMutVar mqTail 0
{-# INLINE mqClear #-}

mqFreeze :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (U.Vector a)
mqFreeze MutableQueue{..} = do
  v <- readMutVar mqVect
  !t <- readMutVar mqTail
  U.freeze $ UM.unsafeTake t v

mqUnsafeFreeze :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (U.Vector a)
mqUnsafeFreeze MutableQueue{..} = do
  v <- readMutVar mqVect
  !t <- readMutVar mqTail
  U.unsafeFreeze $ UM.unsafeTake t v