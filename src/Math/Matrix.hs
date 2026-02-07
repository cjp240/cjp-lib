{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Math.Matrix where
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Math.SemiRing

data Matrix a = Matrix
  { matHeight :: !Int,
    matWidth :: !Int,
    matVec :: !(U.Vector a)
  }

instance (U.Unbox a, SemiRing a) => Num (Matrix a) where
  (+) = _matAdd
  (*) = _matMul
  (-) = undefined
  negate = undefined
  fromInteger = undefined
  abs = undefined
  signum = undefined
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}

_matAdd :: (U.Unbox a, SemiRing a) => Matrix a -> Matrix a -> Matrix a
_matAdd (Matrix !ha !wa !a) (Matrix !hb !wb !b)
  | ha /= hb || wa /= wb = error "matAdd : invalid size"
  | otherwise = 
      let !c = U.zipWith (<+>) a b
      in Matrix ha wa c
{-# INLINE _matAdd #-}

matSub :: (U.Unbox a, Ring a) => Matrix a -> Matrix a -> Matrix a
matSub (Matrix !ha !wa !a) (Matrix !hb !wb !b)
  | ha /= hb || wa /= wb = error "subMat : invalid size"
  | otherwise = 
      let !c = U.zipWith (<->) a b
      in Matrix ha wa c
{-# INLINE matSub #-}

_matMul :: (U.Unbox a, SemiRing a) => Matrix a -> Matrix a -> Matrix a
_matMul (Matrix !ha !wa !va) b@(Matrix !hb !wb _)
  | wa /= hb = error "matMul : invalid size"
  | otherwise = runST $ do
      let !bT = matTranspose b
          !vbT = matVec bT
      mc <- UM.unsafeNew (ha * wb)
      forLoop 0 (== ha) succ $ \ !r -> do
        let !offsetA = r * wa
            !rowA = U.unsafeSlice offsetA wa va
        forLoop 0 (== wb) succ \ !c -> do
          let !offsetB = c * hb
              !colB = U.unsafeSlice offsetB hb vbT
              !res = _matDotProduct rowA colB
              !idx = r * wb + c
          UM.unsafeWrite mc idx res
      Matrix ha wb <$> U.unsafeFreeze mc
{-# INLINE _matMul #-}

_matDotProduct :: (U.Unbox a, SemiRing a) => U.Vector a -> U.Vector a -> a
_matDotProduct !v1 !v2 = U.foldl' (\acc (!x, !y) -> acc <+> (x <.> y)) rZero $ U.zip v1 v2
{-# INLINE _matDotProduct #-}

matTranspose :: U.Unbox a => Matrix a -> Matrix a
matTranspose Matrix{..} = runST $ do
  v <- UM.new (matHeight * matWidth)
  forLoop 0 (== matWidth) succ $ \ !x -> do
    forLoop 0 (== matHeight) succ $ \ !y -> do
      let !ix = y * matWidth + x
          !ix' = x * matHeight + y
      UM.unsafeWrite v ix' $! U.unsafeIndex matVec ix
  Matrix matWidth matHeight <$> U.unsafeFreeze v
{-# INLINE matTranspose #-}

matIdentity :: (U.Unbox a, SemiRing a) => Int -> Matrix a
matIdentity !n = runST $ do
  !mi <- UM.replicate (n * n) rZero
  forLoop 0 (== n) succ $ \ !i -> do
    let !idx = i * n + i
    UM.unsafeWrite mi idx rOne
  Matrix n n <$> U.unsafeFreeze mi
{-# INLINE matIdentity #-}

matPow :: (U.Unbox a, SemiRing a, Integral k) => Matrix a -> k -> Matrix a
matPow a@(Matrix !ha !wa _) !k
  | k < 0 = error "matPow : negative exponent"
  | ha /= wa = error "matPow : invalid size"
  | k == 0 = matIdentity ha
  | otherwise = go (matIdentity ha) a k
  where
    go !acc !base !n
      | n == 0 = acc
      | odd n = go (acc * base) (base * base) (div n 2)
      | otherwise = go acc (base * base) (div n 2)
{-# INLINABLE matPow #-}

matScaleL :: (U.Unbox a, SemiRing a) => a -> Matrix a -> Matrix a
matScaleL !s Matrix{..} = Matrix matHeight matWidth $! U.map (s <.>) matVec
{-# INLINE matScaleL #-}

matScaleR :: (U.Unbox a, SemiRing a) => a -> Matrix a -> Matrix a
matScaleR !s Matrix{..} = Matrix matHeight matWidth $! U.map (<.> s) matVec
{-# INLINE matScaleR #-}

matMulVect :: (U.Unbox a, U.Unbox b, SemiRing b) => (a -> b -> b) -> Matrix a -> U.Vector b -> U.Vector b
matMulVect !f Matrix{..} !v
  | matWidth /= U.length v = error "matMulVect : invalid size"
  | otherwise = U.generate matHeight $ \ !r -> 
      let !row = U.unsafeSlice (r * matWidth) matWidth matVec
          !res = U.foldl' (\ !acc (!x, !y) -> acc <+> f x y) rZero $ U.zip row v
      in res
{-# INLINE matMulVect #-}
{-# SPECIALIZE matMulVect :: (U.Unbox a, SemiRing a) => (a -> a -> a) -> Matrix a -> U.Vector a -> U.Vector a #-}

matFromVect :: Int -> Int -> U.Vector a -> Matrix a
matFromVect !h !w !v = Matrix h w v
{-# INLINE matFromVect #-}

matElemAt :: U.Unbox a => Matrix a -> Int -> Int -> a
matElemAt Matrix{..} !x !y = U.unsafeIndex matVec (x * matWidth + y)
{-# INLINE matElemAt #-}