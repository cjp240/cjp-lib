{-# LANGUAGE RecordWildCards #-}
module Data.MutableStack where
import Control.Monad.Primitive
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
{-# INLINABLE msPush #-}

msNull :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m Bool
msNull MutableStack{..} = (== 0) <$> readMutVar msTail
{-# INLINE msNull #-}

msTop :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m (Maybe a)
msTop MutableStack{..} = do
  !t <- readMutVar msTail
  if t == 0 then return Nothing
  else do
    !v <- readMutVar msVect
    Just <$> UM.unsafeRead v (t - 1)
{-# INLINE msTop #-}

msClear :: PrimMonad m => MutableStack m a -> m ()
msClear MutableStack{..} = writeMutVar msTail 0
{-# INLINE msClear #-}