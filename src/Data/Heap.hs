{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Data.Heap where
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data MutableHeap m p a = MutableHeap
  { vect :: !(MutVar (PrimState m) (UM.MVector (PrimState m) (p, a))),
    size :: !(MutVar (PrimState m) Int)
  }

new :: (PrimMonad m, UM.Unbox p, UM.Unbox a) => Int -> m (MutableHeap m p a)
new !cap = do
  v <- UM.new (cap + 1)
  vect <- newMutVar v
  size <- newMutVar 0
  return $ MutableHeap vect size
{-# INLINE new #-}
{-# SPECIALIZE new :: (UM.Unbox p, UM.Unbox a) => Int -> ST s (MutableHeap (ST s) p a) #-}
{-# SPECIALIZE new :: (UM.Unbox p, UM.Unbox a) => Int -> IO (MutableHeap IO p a) #-}

push :: (PrimMonad m, UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap m p a -> p -> a -> m ()
push MutableHeap{..} !p !a = do
  !sz <- readMutVar size
  !v <- readMutVar vect
  !v' <- 
    if sz + 1 < UM.length v then return v
    else do
      !newV <- UM.unsafeGrow v (sz + 1)
      writeMutVar vect newV
      return newV

  writeMutVar size $! sz + 1

  let go !i
        | i <= 1 = UM.unsafeWrite v' 1 (p, a)
        | otherwise = do
            let !parent = shiftR i 1
            (!pp, !pa) <- UM.unsafeRead v' parent
            if p < pp then do
              UM.unsafeWrite v' i (pp, pa)
              go parent
            else UM.unsafeWrite v' i (p, a)

  go $ sz + 1
{-# INLINE push #-}
{-# SPECIALIZE push :: (UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap (ST s) p a -> p -> a -> ST s () #-}
{-# SPECIALIZE push :: (UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap IO p a -> p -> a -> IO () #-}

pop :: (PrimMonad m, UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap m p a -> m (Maybe (p, a))
pop MutableHeap{..} = do
  !sz <- readMutVar size
  if sz == 0 then return Nothing
  else do
    !v <- readMutVar vect
    !top <- UM.unsafeRead v 1
    (!lastP, !lastA) <- UM.unsafeRead v sz

    writeMutVar size $! sz - 1

    when (sz - 1 > 0) do
      let go !i
            | shiftL i 1 > sz - 1 = UM.unsafeWrite v i (lastP, lastA)
            | otherwise = do
                let !l = shiftL i 1
                    !r = l .|. 1
                !childIdx <-
                  if r <= sz - 1 then do
                    (!lp, _) <- UM.unsafeRead v l
                    (!rp, _) <- UM.unsafeRead v r
                    return $ if lp < rp then l else r
                  else return l
                
                (!cp, !ca) <- UM.unsafeRead v childIdx
                if cp < lastP then do
                  UM.unsafeWrite v i (cp, ca)
                  go childIdx
                else UM.unsafeWrite v i (lastP, lastA)
      go 1

    return $ Just top
{-# INLINE pop #-}
{-# SPECIALIZE pop :: (UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap (ST s) p a -> ST s (Maybe (p, a)) #-}
{-# SPECIALIZE pop :: (UM.Unbox p, Ord p, UM.Unbox a) => MutableHeap IO p a -> IO (Maybe (p, a)) #-}

null :: PrimMonad m => MutableHeap m p a -> m Bool
null MutableHeap{..} = (== 0) <$> readMutVar size
{-# INLINE null #-}
{-# SPECIALIZE Data.Heap.null :: MutableHeap (ST s) p a -> ST s Bool #-}
{-# SPECIALIZE Data.Heap.null :: MutableHeap IO p a -> IO Bool #-}