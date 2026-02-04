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
{-# SPECIALIZE modBuildFacts :: Int -> Facts998 #-}
{-# SPECIALIZE modBuildFacts :: Int -> Facts107 #-}
{-# SPECIALIZE modBuildFacts :: Int -> Facts754 #-}
{-# SPECIALIZE modBuildFacts :: Int -> Facts167 #-}
{-# SPECIALIZE modBuildFacts :: Int -> Facts469 #-}

nCr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nCr Factorials{..} !n !r
  | r < 0 || n < r = 0
  | otherwise = modFactV U.! n * modInvFactV U.! r * modInvFactV U.! (n - r)
{-# INLINE nCr #-}
{-# SPECIALIZE nCr :: Facts998 -> Int -> Int -> IntMod998 #-}
{-# SPECIALIZE nCr :: Facts107 -> Int -> Int -> IntMod107 #-}
{-# SPECIALIZE nCr :: Facts754 -> Int -> Int -> IntMod754 #-}
{-# SPECIALIZE nCr :: Facts167 -> Int -> Int -> IntMod167 #-}
{-# SPECIALIZE nCr :: Facts469 -> Int -> Int -> IntMod469 #-}

nPr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nPr Factorials{..} !n !r
  | r < 0 || n < r = 0
  | otherwise = modFactV U.! n * modInvFactV U.! (n - r)
{-# INLINE nPr #-}
{-# SPECIALIZE nPr :: Facts998 -> Int -> Int -> IntMod998 #-}
{-# SPECIALIZE nPr :: Facts107 -> Int -> Int -> IntMod107 #-}
{-# SPECIALIZE nPr :: Facts754 -> Int -> Int -> IntMod754 #-}
{-# SPECIALIZE nPr :: Facts167 -> Int -> Int -> IntMod167 #-}
{-# SPECIALIZE nPr :: Facts469 -> Int -> Int -> IntMod469 #-}


nHr :: KnownNat n => Factorials n -> Int -> Int -> IntMod n
nHr !f !n !r
  | n == 0 = if r == 0 then 1 else 0
  | r < 0 = 0
  | otherwise = nCr f (n + r - 1) r
{-# INLINE nHr #-}
{-# SPECIALIZE nHr :: Facts998 -> Int -> Int -> IntMod998 #-}
{-# SPECIALIZE nHr :: Facts107 -> Int -> Int -> IntMod107 #-}
{-# SPECIALIZE nHr :: Facts754 -> Int -> Int -> IntMod754 #-}
{-# SPECIALIZE nHr :: Facts167 -> Int -> Int -> IntMod167 #-}
{-# SPECIALIZE nHr :: Facts469 -> Int -> Int -> IntMod469 #-}

modGetFact :: KnownNat n => Factorials n -> Int -> IntMod n
modGetFact Factorials{..} !n = modFactV U.! n
{-# INLINE modGetFact #-}
{-# SPECIALIZE modGetFact :: Facts998 -> Int -> IntMod998 #-}
{-# SPECIALIZE modGetFact :: Facts107 -> Int -> IntMod107 #-}
{-# SPECIALIZE modGetFact :: Facts754 -> Int -> IntMod754 #-}
{-# SPECIALIZE modGetFact :: Facts167 -> Int -> IntMod167 #-}
{-# SPECIALIZE modGetFact :: Facts469 -> Int -> IntMod469 #-}

modGetInvFact :: KnownNat n => Factorials n -> Int -> IntMod n
modGetInvFact Factorials{..} !n = modInvFactV U.! n
{-# INLINE modGetInvFact #-}
{-# SPECIALIZE modGetInvFact :: Facts998 -> Int -> IntMod998 #-}
{-# SPECIALIZE modGetInvFact :: Facts107 -> Int -> IntMod107 #-}
{-# SPECIALIZE modGetInvFact :: Facts754 -> Int -> IntMod754 #-}
{-# SPECIALIZE modGetInvFact :: Facts167 -> Int -> IntMod167 #-}
{-# SPECIALIZE modGetInvFact :: Facts469 -> Int -> IntMod469 #-}

modGetInv :: KnownNat n => Factorials n -> Int -> IntMod n
modGetInv Factorials{..} !n = modInvFactV U.! n * modFactV U.! (n - 1)
{-# INLINE modGetInv #-}
{-# SPECIALIZE modGetInv :: Facts998 -> Int -> IntMod998 #-}
{-# SPECIALIZE modGetInv :: Facts107 -> Int -> IntMod107 #-}
{-# SPECIALIZE modGetInv :: Facts754 -> Int -> IntMod754 #-}
{-# SPECIALIZE modGetInv :: Facts167 -> Int -> IntMod167 #-}
{-# SPECIALIZE modGetInv :: Facts469 -> Int -> IntMod469 #-}

type Facts998 = Factorials 998244353
type Facts107 = Factorials 1000000007
type Facts754 = Factorials 754974721
type Facts167 = Factorials 167772161
type Facts469 = Factorials 469762049