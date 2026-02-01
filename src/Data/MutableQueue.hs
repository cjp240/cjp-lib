{-# LANGUAGE RecordWildCards #-}
module Data.MutableQueue where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.MutVar
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
{-# INLINE mqNew #-}
{-# SPECIALIZE mqNew :: UM.Unbox a => Int -> ST s (MutableQueue (ST s) a) #-}
{-# SPECIALIZE mqNew :: UM.Unbox a => Int -> IO (MutableQueue IO a) #-}

mqPop :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (Maybe a)
mqPop MutableQueue{..} = do
  !v <- readMutVar mqVect
  !h <- readMutVar mqHead
  !t <- readMutVar mqTail
  if h == t then return Nothing
  else do
    !top <- UM.unsafeRead v h
    writeMutVar mqHead $! h + 1
    return $ Just top
{-# INLINE mqPop #-}
{-# SPECIALIZE mqPop :: UM.Unbox a => MutableQueue (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE mqPop :: UM.Unbox a => MutableQueue IO a -> IO (Maybe a) #-}

mqPush :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> a -> m ()
mqPush MutableQueue{..} !x = do
  !v <- readMutVar mqVect
  !t <- readMutVar mqTail
  !v' <-
    if t < UM.length v then return v
    else do 
      !newV <- UM.unsafeGrow v t
      writeMutVar mqVect newV
      return newV
  UM.unsafeWrite v' t x
  writeMutVar mqTail $! t + 1
{-# INLINE mqPush #-}
{-# SPECIALIZE mqPush :: UM.Unbox a => MutableQueue (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE mqPush :: UM.Unbox a => MutableQueue IO a -> a -> IO () #-}

mqNull :: PrimMonad m => MutableQueue m a -> m Bool
mqNull MutableQueue{..} = (==) <$> readMutVar mqHead <*> readMutVar mqTail
{-# INLINE mqNull #-}
{-# SPECIALIZE mqNull :: MutableQueue (ST s) a -> ST s Bool #-}
{-# SPECIALIZE mqNull :: MutableQueue IO a -> IO Bool #-}