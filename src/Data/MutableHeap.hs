{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Data.MutableHeap where
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableHeap m p a = MutableHeap
  { mhVect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) (p, a))),
    mhSize :: !(MutVar (PrimState m) Int)
  }

mhNew :: (PrimMonad m, UM.Unbox p, UM.Unbox a) => Int -> m (MutableHeap m p a)
mhNew !cap = do
  v <- UM.new (cap + 1)
  vect <- newMutVar v
  size <- newMutVar 0
  return $ MutableHeap vect size

mhPush :: (PrimMonad m, UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap m p a -> p -> a -> m ()
mhPush MutableHeap{..} !p !a = do
  !sz <- readMutVar mhSize
  !v <- readMutVar mhVect
  !v' <- 
    if sz + 1 < UM.length v 
      then return v
      else do
        !newV <- UM.unsafeGrow v (UM.length v)
        writeMutVar mhVect newV
        return newV

  writeMutVar mhSize $! sz + 1

  let go !i
        | i <= 1 = UM.unsafeWrite v' 1 (p, a)
        | otherwise = do
            let !parent = shiftR i 1
            (!pp, !pa) <- UM.unsafeRead v' parent
            if p < pp 
              then do
                UM.unsafeWrite v' i (pp, pa)
                go parent
              else UM.unsafeWrite v' i (p, a)

  go $ sz + 1
{-# INLINABLE mhPush #-}

mhPop :: (PrimMonad m, UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap m p a -> m (Maybe (p, a))
mhPop MutableHeap{..} = do
  !sz <- readMutVar mhSize
  if sz == 0 
    then return Nothing
    else do
      !v <- readMutVar mhVect
      !top <- UM.unsafeRead v 1
      (!lastP, !lastA) <- UM.unsafeRead v sz

      writeMutVar mhSize $! sz - 1

      when (sz - 1 > 0) do
        let go !i
              | shiftL i 1 > sz - 1 = UM.unsafeWrite v i (lastP, lastA)
              | otherwise = do
                  let !l = shiftL i 1
                      !r = l .|. 1
                  !childIdx <-
                    if r <= sz - 1 
                      then do
                        (!lp, _) <- UM.unsafeRead v l
                        (!rp, _) <- UM.unsafeRead v r
                        return $ if lp < rp then l else r
                      else return l
                  
                  (!cp, !ca) <- UM.unsafeRead v childIdx
                  if cp < lastP 
                    then do
                      UM.unsafeWrite v i (cp, ca)
                      go childIdx
                    else UM.unsafeWrite v i (lastP, lastA)
        go 1

      return $ Just top
{-# INLINABLE mhPop #-}

mhNull :: PrimMonad m => MutableHeap m p a -> m Bool
mhNull MutableHeap{..} = (== 0) <$> readMutVar mhSize
{-# INLINE mhNull #-}

mhTop :: (PrimMonad m, UM.Unbox p, UM.Unbox a) => MutableHeap m p a -> m (Maybe (p, a))
mhTop MutableHeap{..} = do
  !sz <- readMutVar mhSize
  if sz == 0 
    then return Nothing
    else do
      !v <- readMutVar mhVect
      Just <$> UM.unsafeRead v 1
{-# INLINE mhTop #-}

mhClear :: PrimMonad m => MutableHeap m p a -> m ()
mhClear MutableHeap{..} = writeMutVar mhSize 0
{-# INLINE mhClear #-}