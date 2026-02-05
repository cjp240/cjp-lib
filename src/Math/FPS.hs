{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Math.FPS where

import Data.Vector.Unboxed qualified as U
import GHC.TypeLits

import Math.Convolution.NTT
import Math.IntMod

newtype FPS n = FPS {fpsVec :: U.Vector (IntMod n)}
type FPS998 = FPS 998244353
type FPS107 = FPS 1000000007
type FPS754 = FPS 754974721
type FPS167 = FPS 167772161
type FPS469 = FPS 469762049

fpsFromList :: KnownNat n => [Int] -> FPS n
fpsFromList = fpsFromVect . U.fromList
{-# INLINE fpsFromList #-}
{-# SPECIALIZE fpsFromList :: [Int] -> FPS998 #-}

fpsFromVect :: KnownNat n => U.Vector Int -> FPS n
fpsFromVect = fpsShrink . FPS . U.map fromIntegral
{-# INLINE fpsFromVect #-}
{-# SPECIALIZE fpsFromVect :: U.Vector Int -> FPS998 #-}

fpsFixLength :: forall n. KnownNat n => Int -> FPS n -> FPS n
fpsFixLength !len FPS{..}
  | U.length fpsVec >= len = FPS $ U.take len fpsVec
  | otherwise = FPS $ fpsVec U.++ U.replicate (len - U.length fpsVec) 0
{-# INLINE fpsFixLength #-}
{-# SPECIALIZE fpsFixLength :: Int -> FPS998 -> FPS998 #-}

fpsShrink :: KnownNat n => FPS n -> FPS n
fpsShrink FPS{..} = FPS $ _nttShrink fpsVec
{-# INLINE fpsShrink #-}
{-# SPECIALIZE fpsShrink :: FPS998 -> FPS998 #-}

fpsCoeffAt :: KnownNat n => Int -> FPS n -> IntMod n
fpsCoeffAt !d FPS{..}
  | d >= U.length fpsVec = 0
  | otherwise = U.unsafeIndex fpsVec d
{-# INLINE fpsCoeffAt #-}
{-# SPECIALIZE fpsCoeffAt :: Int -> FPS998 -> IntMod998 #-}

instance (KnownNat n, NTTConvolvable n) => Num (FPS n) where
  f + g = fpsShrink $ FPS $ U.generate (max (U.length (fpsVec f)) (U.length (fpsVec g))) $ \ !i -> 
    fpsCoeffAt i f + fpsCoeffAt i g
  f - g = fpsShrink $ FPS $ U.generate (max (U.length (fpsVec f)) (U.length (fpsVec g))) $ \ !i -> 
    fpsCoeffAt i f - fpsCoeffAt i g
  FPS !f * FPS !g = FPS $ _nttConvolve f g
  fromInteger !x = 
    let !r = fromInteger x :: IntMod n
    in FPS if r == 0 then U.empty else U.singleton r
  abs = undefined
  signum = undefined
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE fromInteger #-}

fpsMulAt :: (KnownNat n, NTTConvolvable n) => Int -> FPS n -> FPS n -> FPS n
fpsMulAt !limit (FPS !f) (FPS !g) = FPS $ _nttConvolveAt limit f g
{-# INLINE fpsMulAt #-}