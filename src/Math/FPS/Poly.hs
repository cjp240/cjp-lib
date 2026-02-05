{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Math.FPS.Poly where
import Control.Monad.ST
import Data.Bifunctor qualified as BF
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Common.Template
import Math.IntMod
import Math.FPS
import Math.FPS.Sparse

data Poly n = S !(SparseFPS n) | D !(FPS n)
type Poly998 = Poly 998244353
type Poly107 = Poly 1000000007

polyThreshold :: Int
polyThreshold = 64

polyFromVect :: KnownNat n => U.Vector Int -> Poly n
polyFromVect = polyFromDense . fpsFromVect
{-# INLINE polyFromVect #-}

polyFromList :: KnownNat n => [Int] -> Poly n
polyFromList = polyFromVect . U.fromList
{-# INLINE polyFromList #-}

polyFromDense :: KnownNat n => FPS n -> Poly n
polyFromDense f@FPS{..}
  | U.length fpsVec <= polyThreshold = S $ sfpsFromDense f
  | otherwise = D f
{-# INLINE polyFromDense #-}

polyFromSparse :: KnownNat n => SparseFPS n -> Poly n
polyFromSparse s@SparseFPS{..}
  | U.length sfpsVec <= polyThreshold = S s
  | otherwise = D $ sfpsToDense s
{-# INLINE polyFromSparse #-}

polyCoeffAt :: KnownNat n => Int -> Poly n -> IntMod n
polyCoeffAt !d = \case
  S !s -> sfpsCoeffAt d s
  D !f -> fpsCoeffAt d f
{-# INLINE polyCoeffAt #-}

polyDeg :: KnownNat n => Poly n -> Int
polyDeg = \case
  D !d -> fpsDeg d
  S !s -> sfpsDeg s
{-# INLINE polyDeg #-}

instance KnownNat n => Num (Poly n) where
  (+) = _polyAdd
  !p1 - !p2 = p1 + negate p2
  negate (S (SparseFPS !s)) = S $ SparseFPS $ U.map (BF.second negate) s
  negate (D (FPS !d)) = D $ FPS $ U.map negate d
  (*) = _polyMul
  fromInteger = S . fromInteger
  abs = undefined
  signum = undefined
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE (*) #-}
  {-# INLINE fromInteger #-}

_polyAdd :: KnownNat n => Poly n -> Poly n -> Poly n
_polyAdd (S !s1) (S !s2) = polyFromSparse $ s1 + s2
_polyAdd (S !s) (D !d) = _addDenseSparse d s
_polyAdd (D !d) (S !s) = _addDenseSparse d s
_polyAdd (D !d1) (D !d2) = polyFromDense $ d1 + d2
{-# INLINABLE _polyAdd #-}

_addDenseSparse :: KnownNat n => FPS n -> SparseFPS n -> Poly n
_addDenseSparse d@FPS{..} s@SparseFPS{..}
  | U.null fpsVec = S s
  | U.null sfpsVec = D d
  | otherwise = 
      let !lenD = U.length fpsVec
          (!sdMax, _) = U.last sfpsVec
          !dMax = max (lenD - 1) sdMax
          !res = fpsShrink $ FPS $ runST $ do
            v <- UM.replicate (dMax + 1) 0
            U.unsafeCopy (UM.unsafeTake lenD v) fpsVec
            U.forM_ sfpsVec $ \(!di, !vi) -> UM.unsafeModify v (+ vi) di
            U.unsafeFreeze v
      in polyFromDense res
{-# INLINABLE _addDenseSparse #-}

_polyMul :: KnownNat n => Poly n -> Poly n -> Poly n
_polyMul (S !s1) (S !s2) = polyFromSparse $ s1 * s2
_polyMul (S !s) (D !d) = _mulDenseSparse d s
_polyMul (D !d) (S !s) = _mulDenseSparse d s
_polyMul (D !d1) (D !d2) = polyFromDense $ d1 * d2

_mulDenseSparse :: KnownNat n => FPS n -> SparseFPS n -> Poly n
_mulDenseSparse FPS{..} SparseFPS{..}
  | U.null fpsVec || U.null sfpsVec = S 0
  | otherwise = 
      let !lenD = U.length fpsVec
          (!maxS, _) = U.last sfpsVec
          !res = fpsShrink $ FPS $ runST $ do
            v <- UM.replicate (lenD + maxS) 0
            U.forM_ sfpsVec $ \(!i, !si) -> do
              U.iforM_ fpsVec $ \ !j !dj -> do
                UM.unsafeModify v (+ (si * dj)) (i + j)
            U.unsafeFreeze v
      in polyFromDense res
{-# INLINABLE _mulDenseSparse #-}

polyMulAt :: KnownNat n => Int -> Poly n -> Poly n -> Poly n
polyMulAt !limit (S !s1) (S !s2) = polyFromSparse $ sfpsMulAt limit s1 s2
polyMulAt !limit (S !s) (D !d) = _mulDenseSparseAt limit d s
polyMulAt !limit (D !d) (S !s) = _mulDenseSparseAt limit d s
polyMulAt !limit (D !d1) (D !d2) = polyFromDense $ fpsMulAt limit d1 d2
{-# INLINABLE polyMulAt #-}

_mulDenseSparseAt :: KnownNat n => Int -> FPS n -> SparseFPS n -> Poly n
_mulDenseSparseAt !limit (FPS !d) (SparseFPS !s)
  | U.null d' || U.null s' = S 0
  | otherwise = 
      let !lenD = U.length d'
          (!maxS, _) = U.last s'
          !len = min limit (lenD + maxS)
          !res = fpsShrink $ FPS $ runST $ do
            v <- UM.replicate len 0
            U.forM_ s' $ \(!i, !si) -> do
              let !limitJ = min lenD (len - i)
              forLoop 0 (>= limitJ) succ $ \ !j -> do
                let !dj = U.unsafeIndex d' j
                UM.unsafeModify v (+ (si * dj)) (i + j)
            U.unsafeFreeze v
      in polyFromDense res
  where
    !d' = U.take limit d
    !s' = U.takeWhile ((< limit) . fst) s
{-# INLINABLE _mulDenseSparseAt #-}