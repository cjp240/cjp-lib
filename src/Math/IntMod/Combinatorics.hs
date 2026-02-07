{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module  Math.IntMod.Combinatorics where
import GHC.TypeLits
import Data.Vector.Unboxed qualified as U
import Math.IntMod

data Factorials n = Factorials
  { modFactV :: !(U.Vector (IntMod n)),
    modInvFactV :: !(U.Vector (IntMod n))
  }

modBuildFacts :: KnownNat n => Int -> Factorials n
modBuildFacts !n = Factorials fv ifv
  where
    !fv = U.scanl' (*) 1 $ U.map fromIntegral $ U.enumFromN (1 :: Int) n
    !ifv = U.scanr' (*) (recip $ U.last fv) $ U.map fromIntegral $ U.enumFromN (1 :: Int) n
{-# INLINABLE modBuildFacts #-}

nCr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nCr Factorials{..} !n !r
  | r < 0 || n < r = 0
  | otherwise = modFactV U.! n * modInvFactV U.! r * modInvFactV U.! (n - r)
{-# INLINE nCr #-}

nPr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nPr Factorials{..} !n !r
  | r < 0 || n < r = 0
  | otherwise = modFactV U.! n * modInvFactV U.! (n - r)
{-# INLINE nPr #-}


nHr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nHr !f !n !r
  | n == 0 = if r == 0 then 1 else 0
  | r < 0 = 0
  | otherwise = nCr f (n + r - 1) r
{-# INLINE nHr #-}

modGetFact :: KnownNat n => Factorials n -> Int -> IntMod n
modGetFact Factorials{..} !n = modFactV U.! n
{-# INLINE modGetFact #-}

modGetInvFact :: KnownNat n => Factorials n -> Int -> IntMod n
modGetInvFact Factorials{..} !n = modInvFactV U.! n
{-# INLINE modGetInvFact #-}

modGetInv :: KnownNat n => Factorials n -> Int -> IntMod n
modGetInv Factorials{..} !n = modInvFactV U.! n * modFactV U.! (n - 1)
{-# INLINE modGetInv #-}

type Facts998 = Factorials 998244353
type Facts107 = Factorials 1000000007
type Facts754 = Factorials 754974721
type Facts167 = Factorials 167772161
type Facts469 = Factorials 469762049