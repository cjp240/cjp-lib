{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Math.FPS.Poly.Extra where
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Common.Template
import Math.IntMod.Combinatorics
import Math.FPS
import Math.FPS.Extra
import Math.FPS.Sparse
import Math.FPS.Sparse.Extra
import Math.FPS.Poly

polyDiff :: KnownNat n => Poly n -> Poly n
polyDiff = \case
  D !d -> polyFromDense $ fpsDiff d
  S !s -> polyFromSparse $ sfpsDiff s
{-# INLINE polyDiff #-}

polyIntegral :: KnownNat n => Factorials n -> Poly n -> Poly n
polyIntegral !facts = \case
  D !d -> polyFromDense $ fpsIntegral facts d
  S !s -> polyFromSparse $ sfpsIntegral facts s
{-# INLINE polyIntegral #-}

polyInv :: KnownNat n => Int -> Poly n -> Poly n
polyInv !limit = \case
  D !d -> polyFromDense $ fpsInv limit d
  S !s -> polyFromDense $ sfpsInv limit s
{-# INLINE polyInv #-}

polyExp :: KnownNat n => Factorials n -> Int -> Poly n -> Poly n
polyExp !facts !limit = \case
  D !d -> polyFromDense $ fpsExp facts limit d
  S !s -> polyFromDense $ sfpsExp facts limit s
{-# INLINE polyExp #-}

polyLog :: KnownNat n => Factorials n -> Int -> Poly n -> Poly n
polyLog !facts !limit = \case
  D !d -> polyFromDense $ fpsLog facts limit d
  S !s -> polyFromDense $ sfpsLog facts limit s
{-# INLINE polyLog #-}

polyPow :: (KnownNat n, Integral k) => Factorials n -> Int -> k -> Poly n -> Poly n
polyPow !facts !limit !k = \case
  D !d -> polyFromDense $ fpsPow facts limit k d
  S !s -> polyFromDense $ sfpsPow facts limit k s
{-# INLINE polyPow #-}

polySqrt :: KnownNat n => Factorials n -> Int -> Poly n -> Maybe (Poly n)
polySqrt !facts !limit = \case
  D !d -> do
    !f <- fpsSqrt limit d
    return $ polyFromDense f
  S !s -> do
    !f <- sfpsSqrt facts limit s
    return $ polyFromDense f
{-# INLINE polySqrt #-}

polyDivMod :: KnownNat n => Poly n -> Poly n -> (Poly n, Poly n)
polyDivMod (S !s1) (S !s2) = _divModSparse s1 s2
polyDivMod (S !s) (D !d) = polyDivMod (D (sfpsToDense s)) (D d)
polyDivMod (D !d) (S !s) = _divModDenseSparse d s
polyDivMod (D !d1) (D !d2) = let (!q, !r) = fpsDivMod d1 d2 in (polyFromDense q, polyFromDense r)
{-# INLINE polyDivMod #-}

_divModSparse :: KnownNat n => SparseFPS n -> SparseFPS n -> (Poly n, Poly n)
_divModSparse f@(SparseFPS !fv) g@(SparseFPS !gv)
  | degG == -1 = error "polyDivMod : division by zero"
  | degF < degG = (S 0, S f)
  | otherwise = runST $ do
      let (!m, !gm) = U.last gv
          !invGm = recip gm
          !gRest = U.init gv
          !degQ = degF - degG
      
      r <- UM.replicate (degF + 1) 0
      U.forM_ fv $ \(!d, !v) -> UM.unsafeWrite r d v
      q <- UM.replicate (degQ + 1) 0
      forLoop degF (< m) pred $ \ !i -> do
        !ri <- UM.unsafeRead r i
        when (ri /= 0) do
          let !qi = ri * invGm
              !qiDeg = i - m
          UM.unsafeWrite q qiDeg qi
          U.forM_ gRest $ \(!gDeg, !gVal) -> do
            UM.unsafeModify r (subtract (qi * gVal)) (qiDeg + gDeg)
          UM.unsafeWrite r i 0
      
      !resQ <- polyFromDense . FPS <$> U.unsafeFreeze q
      !resR <- polyFromDense . FPS <$> U.unsafeFreeze r
      return (resQ, resR)
  where
    !degF = sfpsDeg f
    !degG = sfpsDeg g
{-# INLINABLE _divModSparse #-}

_divModDenseSparse :: KnownNat n => FPS n -> SparseFPS n -> (Poly n, Poly n)
_divModDenseSparse f@(FPS !fv) g@(SparseFPS !gv)
  | degG == -1 = error "polyDivMod: division by zero"
  | degF < degG = (S 0, D f)
  | otherwise = runST $ do
      let (!m, !gm) = U.last gv
          !invGm = recip gm
          !gRest = U.init gv
          !degQ = degF - degG
      
      r <- U.thaw fv
      q <- UM.replicate (degQ + 1) 0
      forLoop degF (< m) pred $ \ !i -> do
        !ri <- UM.unsafeRead r i
        when (ri /= 0) do
          let !qi = ri * invGm
              !qiDeg = i - m
          UM.unsafeWrite q qiDeg qi
          U.forM_ gRest $ \(!gDeg, !gVal) -> do
            UM.unsafeModify r (subtract (qi * gVal)) (qiDeg + gDeg)
          UM.unsafeWrite r i 0

      !resQ <- polyFromDense . FPS <$> U.unsafeFreeze q
      !resR <- polyFromDense . FPS <$> U.unsafeFreeze r
      return (resQ, resR)
  where
    !degF = fpsDeg f
    !degG = sfpsDeg g
{-# INLINABLE _divModDenseSparse #-}