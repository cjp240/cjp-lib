{-# LANGUAGE RecordWildCards #-}
module Data.Queue where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableQueue m a = MutableQueue
  { vect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    qHead :: !(MutVar (PrimState m) Int),
    qTail :: !(MutVar (PrimState m) Int)
  }

new :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableQueue m a)
new !sz = do
  v <- UM.new sz
  vect <- newMutVar v
  h <- newMutVar 0
  t <- newMutVar 0
  return $ MutableQueue vect h t
{-# INLINE new #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> ST s (MutableQueue (ST s) a) #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> IO (MutableQueue IO a) #-}

pop :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> m (Maybe a)
pop MutableQueue{..} = do
  !v <- readMutVar vect
  !h <- readMutVar qHead
  !t <- readMutVar qTail
  if h == t then return Nothing
  else do
    !top <- UM.unsafeRead v h
    writeMutVar qHead $! h + 1
    return $ Just top
{-# INLINE pop #-}
{-# SPECIALIZE pop :: UM.Unbox a => MutableQueue (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE pop :: UM.Unbox a => MutableQueue IO a -> IO (Maybe a) #-}

push :: (PrimMonad m, UM.Unbox a) => MutableQueue m a -> a -> m ()
push MutableQueue{..} !x = do
  !v <- readMutVar vect
  !t <- readMutVar qTail
  !v' <-
    if t < UM.length v then return v
    else do 
      !newV <- UM.unsafeGrow v t
      writeMutVar vect newV
      return newV
  UM.unsafeWrite v' t x
  writeMutVar qTail $! t + 1
{-# INLINE push #-}
{-# SPECIALIZE push :: UM.Unbox a => MutableQueue (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE push :: UM.Unbox a => MutableQueue IO a -> a -> IO () #-}

null :: PrimMonad m => MutableQueue m a -> m Bool
null MutableQueue{..} = (==) <$> readMutVar qHead <*> readMutVar qTail
{-# INLINE null #-}
{-# SPECIALIZE Data.Queue.null :: MutableQueue (ST s) a -> ST s Bool #-}
{-# SPECIALIZE Data.Queue.null :: MutableQueue IO a -> IO Bool #-}