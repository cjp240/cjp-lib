{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.SegTree.SegTree where
import Control.Monad
import Control.Monad.Extra
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

data SegTree m a
  = SegTree
  { segN :: !Int,
    segSize :: !Int,
    segLog :: !Int,
    segNode :: !(UM.MVector (PrimState m) a),
    segOp :: !(a -> a -> a),
    segUnit :: !a
  }

segNew :: (PrimMonad m, UM.Unbox a) => Int -> (a -> a -> a) -> a -> m (SegTree m a)
segNew !n !op !sUnit = do
  let !sLog = if n <= 1 then 0 else finiteBitSize (n - 1) - countLeadingZeros (n - 1)
      !size = shiftL 1 sLog
  node <- UM.replicate (2 * size) sUnit
  return $ SegTree n size sLog node op sUnit
{-# INLINE segNew #-}

segFromVect :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> U.Vector a -> m (SegTree m a)
segFromVect !op !sUnit !v = do
  let n = U.length v
  st <- segNew n op sUnit

  U.iforM_ v $ \ !i !x -> do
    UM.unsafeWrite (segNode st) (segSize st + i) x

  forLoop (segSize st - 1) (== 0) pred $ \ !i -> do
    _segPull st i

  return st
{-# INLINE segFromVect #-}

segFromList :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> [a] -> m (SegTree m a)
segFromList !op !sUnit !xs = segFromVect op sUnit $ U.fromList xs
{-# INLINE segFromList #-}

_segPull :: (PrimMonad m, UM.Unbox a) => SegTree m a -> Int -> m ()
_segPull SegTree{..} !k = do
  !nl <- UM.unsafeRead segNode (shiftL k 1)
  !nr <- UM.unsafeRead segNode (shiftL k 1 .|. 1)
  UM.unsafeWrite segNode k $! segOp nl nr
{-# INLINE _segPull #-}
{-# SPECIALIZE _segPull :: UM.Unbox a => SegTree (ST s) a -> Int -> ST s () #-}
{-# SPECIALIZE _segPull :: UM.Unbox a => SegTree IO a -> Int -> IO () #-}

segSet :: (PrimMonad m, UM.Unbox a) => SegTree m a -> Int -> a -> m ()
segSet st@SegTree{..} !p !x = do
  unless (0 <= p && p < segN) do error $ "segSet : index out of bounds" ++ show p
  let !p0 = p + segSize
  UM.unsafeWrite segNode p0 x
  forLoop p0 (== 1) (`shiftR` 1) $ \ !i -> do
    let !parent = shiftR i 1
    _segPull st parent
{-# INLINE segSet #-}
{-# SPECIALIZE segSet :: UM.Unbox a => SegTree (ST s) a -> Int -> a -> ST s () #-}
{-# SPECIALIZE segSet :: UM.Unbox a => SegTree IO a -> Int -> a -> IO () #-}

segModify :: (PrimMonad m, UM.Unbox a) => SegTree m a -> Int -> (a -> a) -> m ()
segModify st@SegTree{..} !p !f = do
  unless (0 <= p && p < segN) do error $ "segModify : index out of bounds" ++ show p
  let !p0 = p + segSize
  UM.unsafeModify segNode f p0
  forLoop p0 (== 1) (`shiftR` 1) $ \ !i -> do
    let !parent = shiftR i 1
    _segPull st parent
{-# INLINE segModify #-}
{-# SPECIALIZE segModify :: UM.Unbox a => SegTree (ST s) a -> Int -> (a -> a) -> ST s () #-}
{-# SPECIALIZE segModify :: UM.Unbox a => SegTree IO a -> Int -> (a -> a) -> IO () #-}

segRead :: (PrimMonad m, UM.Unbox a) => SegTree m a -> Int -> m a
segRead SegTree{..} !p = do 
  unless (0 <= p && p < segN) do error $ "segRead : index out of bounds" ++ show p
  UM.unsafeRead segNode (p + segSize)
{-# INLINE segRead #-}
{-# SPECIALIZE segRead :: UM.Unbox a => SegTree (ST s) a -> Int -> ST s a #-}
{-# SPECIALIZE segRead :: UM.Unbox a => SegTree IO a -> Int -> IO a #-}

segProd :: (PrimMonad m, UM.Unbox a) => SegTree m a -> Int -> Int -> m a
segProd SegTree{..} l r = do
  unless (0 <= l && l <= r && r <= segN) do error "segProd : index out of bounds"
  let !l0 = l + segSize
      !r0 = r + segSize
  
      go !i !j !accI !accJ
        | i >= j = return $! segOp accI accJ
        | otherwise = do
            (!i', !accI') <- 
              if odd i then do
                !ni <- UM.unsafeRead segNode i
                return (i + 1, segOp accI ni)
              else return (i, accI)
            (!j', !accJ') <- 
              if odd j then do
                !nj <- UM.unsafeRead segNode (j - 1)
                return (j - 1, segOp nj accJ)
              else return (j, accJ)
            
            go (shiftR i' 1) (shiftR j' 1) accI' accJ'

  go l0 r0 segUnit segUnit
{-# INLINE segProd #-}
{-# SPECIALIZE segProd :: UM.Unbox a => SegTree (ST s) a -> Int -> Int -> ST s a #-}
{-# SPECIALIZE segProd :: UM.Unbox a => SegTree IO a -> Int -> Int -> IO a #-}

segAllProd :: (PrimMonad m, UM.Unbox a) => SegTree m a -> m a
segAllProd SegTree{..} = UM.unsafeRead segNode 1
{-# INLINE segAllProd #-}
{-# SPECIALIZE segAllProd :: UM.Unbox a => SegTree (ST s) a -> ST s a #-}
{-# SPECIALIZE segAllProd :: UM.Unbox a => SegTree IO a -> IO a #-}

segMaxR :: (PrimMonad m, UM.Unbox a) => 
  SegTree m a -> (a -> Bool) -> Int -> m Int
segMaxR st f = segMaxRM st (pure . f)
{-# INLINE segMaxR #-}

segMaxRM :: (PrimMonad m, UM.Unbox a) =>
  SegTree m a -> (a -> m Bool) -> Int -> m Int
segMaxRM SegTree{..} f l = do

  unless (0 <= l && l <= segN) do error "segMaxR : index out of bounds"
  unlessM (f segUnit) do error "segMaxR : not suitable on unit"
  if l == segN then return segN
  else do
    let !l0 = l + segSize

        up !i !acc = do
          let !x = shiftR i (countTrailingZeros i)
          !nx <- UM.unsafeRead segNode x
          let !acc' = segOp acc nx
          ifM (f acc')
            do
              let !x' = x + 1
              if x' .&. (- x') == x' then return segN
              else up x' acc'
            do
              down x acc
        
        down !x !acc = do
          if x >= segSize then return $ x - segSize
          else do
            let !left = shiftL x 1
            !nl <- UM.unsafeRead segNode left
            let !accL = segOp acc nl
            ifM (f accL)
              do down (left .|. 1) accL
              do down left acc
  
    up l0 segUnit
{-# SPECIALIZE segMaxRM :: UM.Unbox a => SegTree (ST s) a -> (a -> ST s Bool) -> Int -> ST s Int #-}
{-# SPECIALIZE segMaxRM :: UM.Unbox a => SegTree IO a -> (a -> IO Bool) -> Int -> IO Int #-}

segMinL :: (PrimMonad m, UM.Unbox a) => 
  SegTree m a -> (a -> Bool) -> Int -> m Int
segMinL st f = segMinLM st (pure . f)
{-# INLINE segMinL #-}

segMinLM :: (PrimMonad m, UM.Unbox a) =>
  SegTree m a -> (a -> m Bool) -> Int -> m Int
segMinLM SegTree{..} f r = do
  unless (0 <= r && r <= segN) do error "segMinL : index out of bounds"
  unlessM (f segUnit) do error "segMinL : not suitable on unit"

  if r == 0 then return 0
  else do
    let !r0 = r + segSize

        up !i !acc = do
          let !i' = i - 1
              !x = until (\ !k -> k <= 1 || even k) (`shiftR` 1) i'
          !nx <- UM.unsafeRead segNode x
          let !acc' = segOp nx acc
          ifM (f acc')
            do 
              if (x .&. (- x)) == x then return 0
              else up x acc'
            do down x acc

        down !x !acc = do
          if x >= segSize then return $ x + 1 - segSize
          else do
            let !right = shiftL x 1 .|. 1
            !nr <- UM.unsafeRead segNode right
            let !accR = segOp nr acc
            ifM (f accR)
              do down (right - 1) accR
              do down right acc
    
    up r0 segUnit
{-# SPECIALIZE segMinLM :: UM.Unbox a => SegTree (ST s) a -> (a -> ST s Bool) -> Int -> ST s Int #-}
{-# SPECIALIZE segMinLM :: UM.Unbox a => SegTree IO a -> (a -> IO Bool) -> Int -> IO Int #-}