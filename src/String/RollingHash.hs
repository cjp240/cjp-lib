module String.RollingHash where
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Word
import System.Random

_rhMask30 :: Word64
_rhMask30 = shiftL 1 30 - 1
_rhMask31 :: Word64
_rhMask31 = shiftL 1 31 - 1
_rhMask61 :: Word64
_rhMask61 = shiftL 1 61 - 1

rhGenBase :: IO Word64
rhGenBase = do
  !g <- getStdGen
  let (!b, _) = randomR (2, _rhMask61 - 2) g
  return b

_rhMult :: Word64 -> Word64 -> Word64
_rhMult !x !y = z
  where
    (!xu, !xd) = (shiftR x 31, x .&. _rhMask31)
    (!yu, !yd) = (shiftR y 31, y .&. _rhMask31)
    !mid = xd * yu + xu * yd
    (!midu, !midd) = (shiftR mid 30, mid .&. _rhMask30)
    !z = xu * yu * 2 + midu + shiftL midd 31 + xd * yd
{-# INLINE _rhMult #-}

_rhMult' :: Word64 -> Word64 -> Word64
_rhMult' !x !y = _rhCalcMod $ _rhMult x y
{-# INLINE _rhMult' #-}

_rhCalcMod :: Word64 -> Word64
_rhCalcMod !x = xmod
  where
    (!xu, !xd) = (shiftR x 61, x .&. _rhMask61)
    !res = xu + xd
    !xmod | res >= _rhMask61 = res - _rhMask61
          | otherwise = res
{-# INLINE _rhCalcMod #-}

_rhPow :: Word64 -> Int -> Word64
_rhPow !x !k
  | k < 0 = undefined
  | k == 0 = 1
  | even k = _rhPow (_rhMult' x x) (shiftR k 1)
  | otherwise = _rhMult' x $ _rhPow x (k - 1)
{-# INLINE _rhPow #-}

rhCalcHash :: Word64 -> U.Vector Word64 -> U.Vector Word64
rhCalcHash !b = U.scanl' (\ !h !x -> _rhCalcMod $ _rhMult h b + x) 0
{-# INLINE rhCalcHash #-}

rhRange :: Word64 -> U.Vector Word64 -> Int -> Int -> Word64
rhRange !b !hash !l !r = _rhCalcMod (U.unsafeIndex hash r + _rhMask61 - _rhMult' (U.unsafeIndex hash l) (_rhPow b (r - l)))

rhConcat :: Word64 -> Word64 -> Word64 -> Int -> Word64
rhConcat !b !h1 !h2 !h2Len = _rhCalcMod (_rhMult h1 (_rhPow b h2Len) + h2)
{-# INLINE rhConcat #-}