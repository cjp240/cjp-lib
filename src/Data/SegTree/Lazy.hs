{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.SegTree.Lazy where
import Control.Monad
import Control.Monad.Extra
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

data SegTreeLazy m a f
  = SegTreeLazy
  {
    seglN :: !Int,
    seglSize :: !Int,
    seglLog :: !Int,

    seglOp :: !(a -> a -> a),
    seglUnit :: !a,
    seglNode :: !(UM.MVector (PrimState m) a),

    seglComposition :: !(f -> f -> f),
    seglUnitL :: !f,
    seglNodeL :: !(UM.MVector (PrimState m) f),
    seglApply :: !(a -> f -> a)
  }

seglNew :: (PrimMonad m, UM.Unbox a,  UM.Unbox f) => 
  Int -> (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> a) -> m (SegTreeLazy m a f)
seglNew !n !op !sUnit !composition !unitL !apply = do
  let !sLog = if n <= 1 then 0 else finiteBitSize n - countLeadingZeros (n - 1)
      !size = shiftL 1 sLog

  node <- UM.replicate (2 * size) sUnit
  lazy <- UM.replicate size unitL

  return $ SegTreeLazy n size sLog op sUnit node composition unitL lazy apply
{-# INLINE seglNew #-}

seglFromVect :: (PrimMonad m, UM.Unbox a, UM.Unbox f) => 
  (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> a) -> U.Vector a -> m (SegTreeLazy m a f)
seglFromVect !op !sUnit !composition !unitL !apply !v = do
  let !n = U.length v
  st <- seglNew n op sUnit composition unitL apply

  U.iforM_ v $ \ !i !x -> do
    UM.unsafeWrite (seglNode st) (seglSize st + i) x

  forLoop (seglSize st - 1) (== 0) pred $ \ !i -> _seglPull st i

  return st
{-# INLINE seglFromVect #-}

seglFromList :: (PrimMonad m, UM.Unbox a, UM.Unbox f) => 
  (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> a) -> [a] -> m (SegTreeLazy m a f)
seglFromList !op !sUnit !composition !unitL !apply !xs = seglFromVect op sUnit composition unitL apply (U.fromList xs)
{-# INLINE seglFromList #-}

_seglPull :: (PrimMonad m, UM.Unbox a) => SegTreeLazy m a f -> Int -> m ()
_seglPull SegTreeLazy{..} !k = do
  !nl <- UM.unsafeRead seglNode (shiftL k 1)
  !nr <- UM.unsafeRead seglNode (shiftL k 1 .|. 1)
  UM.unsafeWrite seglNode k $! seglOp nl nr
{-# INLINE _seglPull #-}
{-# SPECIALIZE _seglPull :: UM.Unbox a => SegTreeLazy (ST s) a f -> Int -> ST s () #-}
{-# SPECIALIZE _seglPull :: UM.Unbox a => SegTreeLazy IO a f -> Int -> IO () #-}

_seglAllApply :: (PrimMonad m, UM.Unbox a, UM.Unbox f) => SegTreeLazy m a f -> Int -> f -> m ()
_seglAllApply SegTreeLazy{..} !k !f = do
  UM.unsafeModify seglNode (`seglApply` f) k

  when (k < seglSize) do
    UM.unsafeModify seglNodeL (`seglComposition` f) k
{-# INLINE _seglAllApply #-}
{-# SPECIALIZE _seglAllApply :: (UM.Unbox a, UM.Unbox f) => SegTreeLazy (ST s) a f -> Int -> f -> ST s () #-}
{-# SPECIALIZE _seglAllApply :: (UM.Unbox a, UM.Unbox f) => SegTreeLazy IO a f -> Int -> f -> IO () #-}
  
_seglPush :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> m ()
_seglPush st@SegTreeLazy{..} !k = do
  lz <- UM.unsafeRead seglNodeL k
  when (lz /= seglUnitL) do
    _seglAllApply st (shiftL k 1) lz
    _seglAllApply st (shiftL k 1 .|. 1) lz
    UM.unsafeWrite seglNodeL k seglUnitL
{-# INLINE _seglPush #-}
{-# SPECIALIZE _seglPush :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> ST s () #-}
{-# SPECIALIZE _seglPush :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> IO () #-}

seglProd :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> Int -> m a
seglProd st@SegTreeLazy{..} !l !r = do
  unless (0 <= l && l <= r && r <= seglN) do error $ "seglProd : index out of bounds" ++ show (l, r)

  if l == r then return seglUnit
  else do
    let !l0 = l + seglSize
        !r0 = r + seglSize

    forLoop seglLog (== 0) pred $ \i -> do
      when (shiftL (shiftR l0 i) i /= l0) do _seglPush st (shiftR l0 i)
      when (shiftL (shiftR r0 i) i /= r0) do _seglPush st (shiftR (r0 - 1) i)

    let go !i !j !accI !accJ
          | i >= j = return $! seglOp accI accJ
          | otherwise = do
              (!i', !accI') <- 
                if odd i then do
                  !ni <- UM.unsafeRead seglNode i
                  return (i + 1, seglOp accI ni)
                else return (i, accI)
              (!j', !accJ') <- 
                if odd j then do
                  !nj <- UM.unsafeRead seglNode (j - 1)
                  return (j - 1, seglOp nj accJ)
                else return (j, accJ)
              
              go (shiftR i' 1) (shiftR j' 1) accI' accJ'

    go l0 r0 seglUnit seglUnit
{-# INLINE seglProd #-}
{-# SPECIALIZE seglProd :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> Int -> (ST s) a #-}
{-# SPECIALIZE seglProd :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> Int -> IO a #-}

seglAllProd :: (PrimMonad m, UM.Unbox a) => SegTreeLazy m a f -> m a
seglAllProd SegTreeLazy{..} = UM.unsafeRead seglNode 1
{-# INLINE seglAllProd #-}
{-# SPECIALIZE seglAllProd :: UM.Unbox a => SegTreeLazy (ST s) a f -> ST s a #-}
{-# SPECIALIZE seglAllProd :: UM.Unbox a => SegTreeLazy IO a f -> IO a #-}

seglRead :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> m a
seglRead st@SegTreeLazy{..} !p = do
  unless (0 <= p && p < seglN) do error $ "seglRead : index out of bounds" ++ show p
  let !p0 = p + seglSize
  forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR p0 i)
  UM.unsafeRead seglNode p0
{-# INLINE seglRead #-}
{-# SPECIALIZE seglRead :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> ST s a #-}
{-# SPECIALIZE seglRead :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> IO a #-}

seglSet :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> a -> m ()
seglSet st@SegTreeLazy{..} !p !x = do
  unless (0 <= p && p < seglN) do error $ "seglSet : index out of bounds" ++ show p
  let !p0 = p + seglSize
  forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR p0 i)
  UM.unsafeWrite seglNode p0 x
  forLoop 1 (> seglLog) succ $ \i -> _seglPull st (shiftR p0 i)
{-# INLINE seglSet #-}
{-# SPECIALIZE seglSet :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> a -> ST s () #-}
{-# SPECIALIZE seglSet :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> a -> IO () #-}

seglModify :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> (a -> a) -> m ()
seglModify st@SegTreeLazy{..} !p !f = do
  unless (0 <= p && p < seglN) do error $ "seglModify : index out of bounds" ++ show p
  let !p0 = p + seglSize
  forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR p0 i)
  UM.unsafeModify seglNode f p0
  forLoop 1 (> seglLog) succ $ \i -> _seglPull st (shiftR p0 i)
{-# INLINE seglModify #-}
{-# SPECIALIZE seglModify :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> (a -> a) -> ST s () #-}
{-# SPECIALIZE seglModify :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> (a -> a) -> IO () #-}

seglUpdateRange :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> Int -> f -> m ()
seglUpdateRange st@SegTreeLazy{..} !l !r !f = do
  unless (0 <= l && l <= r && r <= seglN) do error $ "seglUpdateRange : index out of bounds" ++ show (l, r)

  if l == r then return ()
  else do
    let !l0 = l + seglSize
        !r0 = r + seglSize
    
    forLoop seglLog (== 0) pred $ \i -> do
      when (shiftL (shiftR l0 i) i /= l0) do _seglPush st (shiftR l0 i)
      when (shiftL (shiftR r0 i) i /= r0) do _seglPush st (shiftR (r0 - 1) i)

    let go !i !j = do
          when (i < j) do
            !i' <- 
              if odd i then do
                _seglAllApply st i f
                return $ i + 1
              else return i
            !j' <- 
              if odd j then do
                _seglAllApply st (j - 1) f
                return $ j - 1
              else return j
            
            go (shiftR i' 1) (shiftR j' 1)
    
    go l0 r0

    forLoop 1 (> seglLog) succ $ \i -> do
      when (shiftL (shiftR l0 i) i /= l0) do _seglPull st (shiftR l0 i)
      when (shiftL (shiftR r0 i) i /= r0) do _seglPull st (shiftR (r0 - 1) i)
{-# SPECIALIZE seglUpdateRange :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> Int -> f -> ST s () #-}
{-# SPECIALIZE seglUpdateRange :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> Int -> f -> IO () #-}

seglUpdate :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> Int -> f -> m ()
seglUpdate st@SegTreeLazy{..} !p !f = do
  unless (0 <= p && p < seglN) do error $ "seglUpdate : index out of bounds" ++ show p
  let !p0 = p + seglSize
  forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR p0 i)
  UM.unsafeModify seglNode (`seglApply` f) p0
  forLoop 1 (> seglLog) succ $ \i -> _seglPull st (shiftR p0 i)
{-# INLINE seglUpdate #-}
{-# SPECIALIZE seglUpdate :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> Int -> f -> ST s () #-}
{-# SPECIALIZE seglUpdate :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> Int -> f -> IO () #-}

seglMaxR :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> (a -> Bool) -> Int -> m Int
seglMaxR st !f = seglMaxRM st (pure . f)
{-# INLINE seglMaxR #-}

seglMaxRM :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> (a -> m Bool) -> Int -> m Int
seglMaxRM st@SegTreeLazy{..} !f !l = do
  unless (0 <= l && l <= seglN) do error $ "maxR : index out of bounds" ++ show l
  unlessM (f seglUnit) do error "maxR : False on seglUnit"

  if l == seglN then return seglN
  else do
    let !l0 = l + seglSize
    forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR l0 i)
    
    let up !i !acc = do
          let !x = shiftR i (countTrailingZeros i)
          !nx <- UM.unsafeRead seglNode x
          let !acc' = seglOp acc nx
          ifM (f acc')
            do
              let !x' = x + 1
              if x' .&. (- x') == x' then return seglN
              else up x' acc'
            do
              down x acc
        
        down !x !acc = do
          if x >= seglSize then return $ x - seglSize
          else do
            _seglPush st x
            let !left = shiftL x 1
            !nl <- UM.unsafeRead seglNode left
            let !accL = seglOp acc nl
            ifM (f accL)
              do down (left .|. 1) accL
              do down left acc

    up l0 seglUnit
{-# SPECIALIZE seglMaxRM :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> (a -> ST s Bool) -> Int -> ST s Int #-}
{-# SPECIALIZE seglMaxRM :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> (a -> IO Bool) -> Int -> IO Int #-}

seglMinL :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> (a -> Bool) -> Int -> m Int
seglMinL !st !f = seglMinLM st (pure . f)
{-# INLINE seglMinL #-}

seglMinLM :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy m a f -> (a -> m Bool) -> Int -> m Int
seglMinLM st@SegTreeLazy{..} !f !r = do
  unless (0 <= r && r <= seglN) do error $ "minL : index out of bounds" ++ show r
  unlessM (f seglUnit) do error "minL : False on seglUnit"
  if r == 0 then return 0
  else do
    let r0 = r + seglSize
    forLoop seglLog (== 0) pred $ \i -> _seglPush st (shiftR (r0 - 1) i)
    
    let up !i !acc = do
          let !i' = i - 1
              !x = until (\ !k -> k <= 1 || even k) (`shiftR` 1) i'
          !nx <- UM.unsafeRead seglNode x
          let !acc' = seglOp nx acc
          ifM (f acc')
            do 
              if (x .&. (- x)) == x then return 0
              else up x acc'
            do down x acc

        down !x !acc = do
          if x >= seglSize then return $ x + 1 - seglSize
          else do
            _seglPush st x
            let !right = shiftL x 1 .|. 1
            !nr <- UM.unsafeRead seglNode right
            let !accR = seglOp nr acc
            ifM (f accR)
              do down (right - 1) accR
              do down right acc
    
    up r0 seglUnit
{-# SPECIALIZE seglMinLM :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy (ST s) a f -> (a -> ST s Bool) -> Int -> ST s Int #-}
{-# SPECIALIZE seglMinLM :: (UM.Unbox a, UM.Unbox f, Eq f) => SegTreeLazy IO a f -> (a -> IO Bool) -> Int -> IO Int #-}