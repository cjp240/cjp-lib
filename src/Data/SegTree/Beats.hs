{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.SegTree.Beats where
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

data SegTreeBeats m a f
  = SegTreeBeats
  {
    segbN :: !Int,
    segbSize :: !Int,
    segbLog :: !Int,

    segbOp :: !(a -> a -> a),
    segbUnit :: !a,
    segbNode :: !(UM.MVector (PrimState m) (a, Bool)),

    segbComposition :: !(f -> f -> f),
    segbUnitL :: !f,
    segbNodeL :: !(UM.MVector (PrimState m) f),
    segbApply :: !(a -> f -> (a, Bool))
  }

segbNew :: (PrimMonad m, UM.Unbox a,  UM.Unbox f) => 
  Int -> (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> (a, Bool)) -> m (SegTreeBeats m a f)
segbNew !n !op !sUnit !composition !unitL !apply = do
  let !sLog = if n <= 1 then 0 else finiteBitSize n - countLeadingZeros (n - 1)
      !size = shiftL 1 sLog

  node <- UM.replicate (2 * size) (sUnit, False)
  lazy <- UM.replicate size unitL

  return $ SegTreeBeats n size sLog op sUnit node composition unitL lazy apply

segbFromVect :: (PrimMonad m, UM.Unbox a, UM.Unbox f) => 
  (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> (a, Bool)) -> U.Vector a -> m (SegTreeBeats m a f)
segbFromVect !op !sUnit !composition !unitL !apply !v = do
  let !n = U.length v
  st <- segbNew n op sUnit composition unitL apply

  U.iforM_ v $ \ !i !x -> do
    UM.unsafeWrite (segbNode st) (segbSize st + i) (x, False)

  forLoop (segbSize st - 1) (== 0) pred $ \ !i -> _segbPull st i

  return st

segbFromList :: (PrimMonad m, UM.Unbox a, UM.Unbox f) => 
  (a -> a -> a) -> a -> (f -> f -> f) -> f -> (a -> f -> (a, Bool)) -> [a] -> m (SegTreeBeats m a f)
segbFromList !op !sUnit !composition !unitL !apply !xs = segbFromVect op sUnit composition unitL apply (U.fromList xs)

_segbPull :: (PrimMonad m, UM.Unbox a) => SegTreeBeats m a f -> Int -> m ()
_segbPull SegTreeBeats{..} !k = do
  (!nl, _) <- UM.unsafeRead segbNode (shiftL k 1)
  (!nr, _) <- UM.unsafeRead segbNode (shiftL k 1 .|. 1)
  let !res = segbOp nl nr
  UM.unsafeWrite segbNode k (res, False)
{-# INLINE _segbPull #-}

_segbAllApply :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> f -> m ()
_segbAllApply st@SegTreeBeats{..} !k !f = do
  (!nk, _) <- UM.unsafeRead segbNode k
  let !nk' = segbApply nk f
  UM.unsafeWrite segbNode k nk'
  when (k < segbSize) do
    !lk <- UM.unsafeRead segbNodeL k
    UM.unsafeWrite segbNodeL k $! segbComposition lk f
    when (snd nk') do
      _segbPush st k
      _segbPull st k
{-# INLINE _segbAllApply #-}
  
_segbPush :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> m ()
_segbPush st@SegTreeBeats{..} !k = do
  !lk <- UM.unsafeRead segbNodeL k
  when (lk /= segbUnitL) do
    _segbAllApply st (shiftL k 1) lk
    _segbAllApply st (shiftL k 1 .|. 1) lk
    UM.unsafeWrite segbNodeL k segbUnitL
{-# INLINE _segbPush #-}

segbProd :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> Int -> m a
segbProd st@SegTreeBeats{..} !l !r = do
  unless (0 <= l && l <= r && r <= segbN) do error $ "segbProd : index out of bounds" ++ show (l, r)

  if l == r 
    then return segbUnit
    else do
      let !l0 = l + segbSize
          !r0 = r + segbSize

      forLoop segbLog (== 0) pred $ \i -> do
        when (shiftL (shiftR l0 i) i /= l0) do _segbPush st (shiftR l0 i)
        when (shiftL (shiftR r0 i) i /= r0) do _segbPush st (shiftR (r0 - 1) i)

      let go !i !j !accI !accJ
            | i >= j = return $! segbOp accI accJ
            | otherwise = do
                (!i', !accI') <- 
                  if odd i 
                    then do
                      (!ni, _) <- UM.unsafeRead segbNode i
                      return (i + 1, segbOp accI ni)
                    else return (i, accI)
                (!j', !accJ') <- 
                  if odd j 
                    then do
                      (!nj, _) <- UM.unsafeRead segbNode (j - 1)
                      return (j - 1, segbOp nj accJ)
                    else return (j, accJ)
                
                go (shiftR i' 1) (shiftR j' 1) accI' accJ'

      go l0 r0 segbUnit segbUnit

segbAllProd :: (PrimMonad m, UM.Unbox a) => SegTreeBeats m a f -> m a
segbAllProd SegTreeBeats{..} = fst <$> UM.unsafeRead segbNode 1
{-# INLINE segbAllProd #-}

segbRead :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> m a
segbRead st@SegTreeBeats{..} !p = do
  unless (0 <= p && p < segbN) do error $ "segbRead : index out of bounds" ++ show p
  let !p0 = p + segbSize
  forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR p0 i)
  fst <$> UM.unsafeRead segbNode p0
{-# INLINE segbRead #-}

segbSet :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> a -> m ()
segbSet st@SegTreeBeats{..} !p !x = do
  unless (0 <= p && p < segbN) do error $ "segbSet : index out of bounds" ++ show p
  let !p0 = p + segbSize
  forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR p0 i)
  UM.unsafeWrite segbNode p0 (x, False)
  forLoop 1 (> segbLog) succ $ \i -> _segbPull st (shiftR p0 i)
{-# INLINE segbSet #-}

segbModify :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> (a -> a) -> m ()
segbModify st@SegTreeBeats{..} !p !f = do
  unless (0 <= p && p < segbN) do error $ "segbModify : index out of bounds" ++ show p
  let p0 = p + segbSize
  forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR p0 i)
  (!np, _) <- UM.unsafeRead segbNode p0
  UM.unsafeWrite segbNode p0 (f np, False)
  forLoop 1 (> segbLog) succ $ \i -> _segbPull st (shiftR p0 i)
{-# INLINE segbModify #-}

segbUpdateRange :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> Int -> f -> m ()
segbUpdateRange st@SegTreeBeats{..} !l !r !f = do
  unless (0 <= l && l <= r && r <= segbN) do error $ "segbUpdateRange : index out of bounds" ++ show (l, r)

  if l == r 
    then return ()
    else do
      let !l0 = l + segbSize
          !r0 = r + segbSize
      
      forLoop segbLog (== 0) pred $ \i -> do
        when (shiftL (shiftR l0 i) i /= l0) do _segbPush st (shiftR l0 i)
        when (shiftL (shiftR r0 i) i /= r0) do _segbPush st (shiftR (r0 - 1) i)

      let go !i !j = do
            when (i < j) do
              !i' <- 
                if odd i 
                  then do
                    _segbAllApply st i f
                    return $ i + 1
                  else return i
              !j' <- 
                if odd j 
                  then do
                    _segbAllApply st (j - 1) f
                    return $ j - 1
                  else return j
              
              go (shiftR i' 1) (shiftR j' 1)
      
      go l0 r0

      forLoop 1 (> segbLog) succ $ \i -> do
        when (shiftL (shiftR l0 i) i /= l0) do _segbPull st (shiftR l0 i)
        when (shiftL (shiftR r0 i) i /= r0) do _segbPull st (shiftR (r0 - 1) i)

segbUpdate :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> Int -> f -> m ()
segbUpdate st@SegTreeBeats{..} !p !f = do
  unless (0 <= p && p < segbN) do error $ "segbUpdate : index out of bounds" ++ show p
  let !p0 = p + segbSize
  forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR p0 i)
  
  (!np, _) <- UM.unsafeRead segbNode p0
  UM.unsafeWrite segbNode p0 $! segbApply np f

  forLoop 1 (> segbLog) succ $ \i -> _segbPull st (shiftR p0 i)
{-# INLINE segbUpdate #-}

segbMaxR :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> (a -> Bool) -> Int -> m Int
segbMaxR st !f = segbMaxRM st (pure . f)
{-# INLINE segbMaxR #-}

segbMaxRM :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> (a -> m Bool) -> Int -> m Int
segbMaxRM st@SegTreeBeats{..} !f !l = do
  unless (0 <= l && l <= segbN) do error $ "maxR : index out of bounds" ++ show l
  unlessM (f segbUnit) do error "maxR : False on segbUnit"

  if l == segbN 
    then return segbN
    else do
      let !l0 = l + segbSize
      forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR l0 i)
      
      let up !i !acc = do
            let !x = shiftR i (countTrailingZeros i)
            (!nx, _) <- UM.unsafeRead segbNode x
            let !acc' = segbOp acc nx
            ifM (f acc')
              do
                let !x' = x + 1
                if x' .&. (- x') == x' 
                  then return segbN
                  else up x' acc'
              do
                down x acc
          
          down !x !acc = do
            if x >= segbSize 
              then return $ x - segbSize
              else do
                _segbPush st x
                let !left = shiftL x 1
                (!nl, _) <- UM.unsafeRead segbNode left
                let !accL = segbOp acc nl
                ifM (f accL)
                  do down (left .|. 1) accL
                  do down left acc

      up l0 segbUnit

segbMinL :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> (a -> Bool) -> Int -> m Int
segbMinL !st !f = segbMinLM st (pure . f)
{-# INLINE segbMinL #-}

segbMinLM :: (PrimMonad m, UM.Unbox a, UM.Unbox f, Eq f) => SegTreeBeats m a f -> (a -> m Bool) -> Int -> m Int
segbMinLM st@SegTreeBeats{..} !f !r = do
  unless (0 <= r && r <= segbN) do error $ "minL : index out of bounds" ++ show r
  unlessM (f segbUnit) do error "minL : False on segbUnit"
  if r == 0 
    then return 0
    else do
      let r0 = r + segbSize
      forLoop segbLog (== 0) pred $ \i -> _segbPush st (shiftR (r0 - 1) i)
      
      let up !i !acc = do
            let !i' = i - 1
                !x = until (\ !k -> k <= 1 || even k) (`shiftR` 1) i'
            (!nx, _) <- UM.unsafeRead segbNode x
            let !acc' = segbOp nx acc
            ifM (f acc')
              do 
                if (x .&. (- x)) == x 
                  then return 0
                  else up x acc'
              do down x acc

          down !x !acc = do
            if x >= segbSize 
              then return $ x + 1 - segbSize
              else do
                _segbPush st x
                let !right = shiftL x 1 .|. 1
                (!nr, _) <- UM.unsafeRead segbNode right
                let !accR = segbOp nr acc
                ifM (f accR)
                  do down (right - 1) accR
                  do down right acc
      
      up r0 segbUnit