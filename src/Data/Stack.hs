{-# LANGUAGE RecordWildCards #-}
module Data.Stack where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableStack m a = MutableStack
  { vect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) a)),
    sTail :: !(MutVar (PrimState m) Int)
  }

new :: (PrimMonad m, UM.Unbox a) => Int -> m (MutableStack m a)
new !sz = do
  v <- UM.new sz
  vect <- newMutVar v
  t <- newMutVar 0
  return $ MutableStack vect t
{-# INLINE new #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> ST s (MutableStack (ST s) a) #-}
{-# SPECIALIZE new :: UM.Unbox a => Int -> IO (MutableStack IO a) #-}

pop :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m (Maybe a)
pop MutableStack{..} = do
  !t <- readMutVar sTail
  if t == 0 then return Nothing
  else do
    !v <- readMutVar vect
    !top <- UM.unsafeRead v (t - 1)
    writeMutVar sTail $! t - 1
    return $ Just top
{-# INLINE pop #-}
{-# SPECIALIZE pop :: UM.Unbox a => MutableStack (ST s) a -> ST s (Maybe a) #-}
{-# SPECIALIZE pop :: UM.Unbox a => MutableStack IO a -> IO (Maybe a) #-}

push :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> a -> m ()
push MutableStack{..} !x = do
  !v <- readMutVar vect
  !t <- readMutVar sTail
  !v' <-
    if t < UM.length v then return v
    else do
      !newV <- UM.unsafeGrow v t
      writeMutVar vect newV
      return newV
  UM.unsafeWrite v' t x
  writeMutVar sTail $! t + 1
{-# INLINE push #-}
{-# SPECIALIZE push :: UM.Unbox a => MutableStack (ST s) a -> a -> ST s () #-}
{-# SPECIALIZE push :: UM.Unbox a => MutableStack IO a -> a -> IO () #-}

null :: (PrimMonad m, UM.Unbox a) => MutableStack m a -> m Bool
null MutableStack{..} = (== 0) <$> readMutVar sTail
{-# INLINE null #-}
{-# SPECIALIZE Data.Stack.null :: UM.Unbox a => MutableStack (ST s) a -> ST s Bool #-}
{-# SPECIALIZE Data.Stack.null :: UM.Unbox a => MutableStack IO a -> IO Bool #-}