{-# LANGUAGE RecordWildCards #-}
module Data.MutableStack where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableStack m a = MutableStack
  { msVect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    msTail :: !(MutVar (PrimState m) Int)
  }

msNew :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableStack m a)
msNew !sz = do
  v <- UM.new sz
  vect <- newMutVar v
  t <- newMutVar 0
  return $ MutableStack vect t
{-# INLINE msNew #-}
{-# SPECIALIZE msNew :: UM.Unbox a => Int -> ST s (MutableStack (ST s) a) #-}
{-# SPECIALIZE msNew :: UM.Unbox a => Int -> IO (MutableStack IO a) #-}

msPop :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m (Maybe a)
msPop MutableStack{..} = do
  !t <- readMutVar msTail
  if t == 0 then return Nothing
  else do
    !v <- readMutVar msVect
    !top <- UM.unsafeRead v (t - 1)
    writeMutVar msTail $! t - 1
    return $ Just top
{-# INLINE msPop #-}
{-# SPECIALIZE msPop :: UM.Unbox a => MutableStack (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE msPop :: UM.Unbox a => MutableStack IO a -> IO (Maybe a) #-}

msPush :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> a -> m ()
msPush MutableStack{..} !x = do
  !v <- readMutVar msVect
  !t <- readMutVar msTail
  !v' <-
    if t < UM.length v then return v
    else do
      !newV <- UM.unsafeGrow v t
      writeMutVar msVect newV
      return newV
  UM.unsafeWrite v' t x
  writeMutVar msTail $! t + 1
{-# INLINE msPush #-}
{-# SPECIALIZE msPush :: UM.Unbox a => MutableStack (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE msPush :: UM.Unbox a => MutableStack IO a -> a -> IO () #-}

msNull :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m Bool
msNull MutableStack{..} = (== 0) <$> readMutVar msTail
{-# INLINE msNull #-}
{-# SPECIALIZE msNull :: UM.Unbox a => MutableStack (ST s) a -> ST s Bool #-}
{-# SPECIALIZE msNull :: UM.Unbox a => MutableStack IO a -> IO Bool #-}