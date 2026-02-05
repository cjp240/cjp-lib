{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Math.IntMod where
import Data.Bits
import Data.Proxy
import Data.Ratio
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import GHC.TypeLits

import Common.IO
import Math.SemiRing

type role IntMod nominal
newtype IntMod (n :: Nat) = IntMod {unIntModInternal :: Int} deriving (Eq, Ord)
instance KnownNat n => Show (IntMod n) where
  show = show . unIntMod
instance KnownNat n => Buildable (IntMod n) where
  toB = toB . unIntMod
  {-# INLINE toB #-}

instance KnownNat n => Num (IntMod n) where
  (IntMod !a) + (IntMod !b) = let !m = modVal @n ; !s = a + b in IntMod (if s >= m then s - m else s)
  (IntMod !a) - (IntMod !b) = let !m = modVal @n ; !d = a - b in IntMod (if d < 0 then d + m else d)
  (*) = multMod
  fromInteger !x
    | m == mod998 = IntMod $ montgomeryReduce998 $ fromIntegral $ fromIntegral v * rMod998
    | m == mod107 = IntMod $ montgomeryReduce107 $ fromIntegral $ fromIntegral v * rMod107
    | m == mod754 = IntMod $ montgomeryReduce754 $ fromIntegral $ fromIntegral v * rMod754
    | m == mod167 = IntMod $ montgomeryReduce167 $ fromIntegral $ fromIntegral v * rMod167
    | m == mod469 = IntMod $ montgomeryReduce469 $ fromIntegral $ fromIntegral v * rMod469
    | otherwise   = IntMod $ fromIntegral v
    where
      !m = modVal @n
      !v = x `mod` fromIntegral m
  abs = id ; signum _ = 1
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE fromInteger #-}

instance KnownNat n => Fractional (IntMod n) where
  recip a = modPow a (modVal @n - 2)
  a / b = a * recip b
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  {-# INLINE recip #-}
  {-# INLINE (/) #-}
  {-# INLINE fromRational #-}

modVal :: forall n. KnownNat n => Int
modVal = fromIntegral (natVal (Proxy @n))
{-# INLINE  modVal #-}

multMod :: forall n. KnownNat n => IntMod n -> IntMod n -> IntMod n
multMod (IntMod !a) (IntMod !b)
  | m == mod998 = IntMod (montgomeryReduce998 (fromIntegral a * fromIntegral b))
  | m == mod107 = IntMod (montgomeryReduce107 (fromIntegral a * fromIntegral b))
  | m == mod754 = IntMod (montgomeryReduce754 (fromIntegral a * fromIntegral b))
  | m == mod167 = IntMod (montgomeryReduce167 (fromIntegral a * fromIntegral b))
  | m == mod469 = IntMod (montgomeryReduce469 (fromIntegral a * fromIntegral b))
  | otherwise = IntMod (fromIntegral (rem ((fromIntegral a :: Integer) * fromIntegral b) (fromIntegral m)))
  where
    !m = modVal @n
{-# INLINE multMod #-}
{-# SPECIALIZE multMod :: IntMod998 -> IntMod998 -> IntMod998 #-}
{-# SPECIALIZE multMod :: IntMod107 -> IntMod107 -> IntMod107 #-}
{-# SPECIALIZE multMod :: IntMod754 -> IntMod754 -> IntMod754 #-}
{-# SPECIALIZE multMod :: IntMod167 -> IntMod167 -> IntMod167 #-}
{-# SPECIALIZE multMod :: IntMod469 -> IntMod469 -> IntMod469 #-}

montgomeryReduce :: Int -> Word32 -> Word64 -> Int
montgomeryReduce !modVal_ !mInv !t = 
  let !u = (fromIntegral t :: Word32) * mInv
      !uM = (fromIntegral u :: Word64) * fromIntegral modVal_
      !res = unsafeShiftR (t + uM) 32
      !resInt = fromIntegral res
  in if resInt >= modVal_ then resInt - modVal_ else resInt
{-# INLINE montgomeryReduce #-}

montgomeryReduce998 :: Word64 -> Int
montgomeryReduce998 = montgomeryReduce mod998 mInv998
{-# INLINE montgomeryReduce998 #-}
montgomeryReduce107 :: Word64 -> Int
montgomeryReduce107 = montgomeryReduce mod107 mInv107
{-# INLINE montgomeryReduce107 #-}
montgomeryReduce754 :: Word64 -> Int
montgomeryReduce754 = montgomeryReduce mod754 mInv754
{-# INLINE montgomeryReduce754 #-}
montgomeryReduce167 :: Word64 -> Int
montgomeryReduce167 = montgomeryReduce mod167 mInv167
{-# INLINE montgomeryReduce167 #-}
montgomeryReduce469 :: Word64 -> Int
montgomeryReduce469 = montgomeryReduce mod469 mInv469
{-# INLINE montgomeryReduce469 #-}

unIntMod :: forall n. KnownNat n => IntMod n -> Int
unIntMod (IntMod !a)
  | m == mod998 = montgomeryReduce998 (fromIntegral a)
  | m == mod107 = montgomeryReduce107 (fromIntegral a)
  | m == mod754 = montgomeryReduce754 (fromIntegral a)
  | m == mod167 = montgomeryReduce167 (fromIntegral a)
  | m == mod469 = montgomeryReduce469 (fromIntegral a)
  | otherwise = a
  where
    !m = modVal @n
{-# INLINE unIntMod #-}
{-# SPECIALIZE unIntMod :: IntMod998 -> Int #-}
{-# SPECIALIZE unIntMod :: IntMod107 -> Int #-}
{-# SPECIALIZE unIntMod :: IntMod754 -> Int #-}
{-# SPECIALIZE unIntMod :: IntMod167 -> Int #-}
{-# SPECIALIZE unIntMod :: IntMod469 -> Int #-}

modPow :: (KnownNat n, Integral k) => IntMod n -> k -> IntMod n
modPow !a !n
  | n < 0 = error "modPow : negative exponent"
  | n == 0 = 1
  | otherwise = go a n 1
  where
    go !b !e !acc
      | e == 0 = acc
      | odd e = go (b * b) (div e 2) (acc * b)
      | otherwise = go (b * b) (div e 2) acc
{-# INLINABLE modPow #-}
{-# SPECIALIZE modPow :: IntMod998 -> Int -> IntMod998 #-}
{-# SPECIALIZE modPow :: IntMod998 -> Integer -> IntMod998 #-}
{-# SPECIALIZE modPow :: IntMod107 -> Int -> IntMod107 #-}
{-# SPECIALIZE modPow :: IntMod107 -> Integer -> IntMod107 #-}

tonelliShanks :: forall n. KnownNat n => IntMod n -> Maybe (IntMod n)
tonelliShanks 0 = Just 0
tonelliShanks 1 = Just 1
tonelliShanks !a = 
  let !p = fromIntegral $ modVal @n :: Integer
      !expo = shiftR (p - 1) 1
      !check = modPow a expo
  in 
    if check == 1
     then if p .&. 3 == 3 
      then Just $ modPow a (shiftR (p + 1) 2)
      else Just $ solve p a
     else Nothing
  where
    trailingZeros :: Integer -> Int
    trailingZeros !n | n == 0 = 0
                     | testBit n 0 = 0
                     | otherwise = 1 + trailingZeros (shiftR n 1)

    solve !p !a_ = 
      let !s = trailingZeros $ p - 1
          !q = shiftR (p - 1) s

          findZ (!z_ :: Int)
            | modPow zn (shiftR (p - 1) 1) == fromIntegral (p - 1) = zn
            | otherwise = findZ $ z_ + 1
            where
              !zn = fromIntegral z_ :: IntMod n
          !z = findZ 2

          !c = modPow z q
          !r = modPow a_ (shiftR (q + 1) 1)
          !t = modPow a_ q
          !m = s

          go !curM !curC !curR !curT
            | curT == 1 = curR
            | otherwise = 
                let findI !i_ !tt
                      | tt == 1 = i_
                      | otherwise = findI (i_ + 1) (tt * tt)
                    !i = findI 1 (curT * curT)
                    !expB = shiftL (1 :: Integer) (curM - i - 1)
                    !b = modPow curC expB
                    !nxtC = b * b
                    !nxtT = curT * nxtC
                    !nxtR = curR * b
                in go i nxtC nxtR nxtT
      in go m c r t

type IntMod998 = IntMod 998244353
type IntMod107 = IntMod 1000000007
type IntMod754 = IntMod 754974721
type IntMod167 = IntMod 167772161
type IntMod469 = IntMod 469762049

mod998, mod107, mod754, mod167, mod469 :: Int
mod998 = 998244353
mod107 = 1000000007
mod754 = 754974721
mod167 = 167772161
mod469 = 469762049

mInv998, mInv107, mInv754, mInv167, mInv469 :: Word32
mInv998 = 998244351
mInv107 = 2226617417
mInv754 = 754974719
mInv167 = 167772159
mInv469 = 469762047

rMod998, rMod107, rMod754, rMod167, rMod469 :: Word64
rMod998 = 932051910
rMod107 = 582344008
rMod754 = 749009521
rMod167 = 40265974
rMod469 = 460175152

newtype instance UM.MVector s (IntMod n) = MV_IntMod (UM.MVector s Int)
newtype instance U.Vector (IntMod n)    = V_IntMod (U.Vector Int)
instance KnownNat n => U.Unbox (IntMod n)
instance KnownNat n => GM.MVector UM.MVector (IntMod n) where
  basicLength (MV_IntMod v) = GM.basicLength v
  basicUnsafeSlice i l (MV_IntMod v) = MV_IntMod (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_IntMod v1) (MV_IntMod v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_IntMod <$> GM.basicUnsafeNew l
  basicInitialize (MV_IntMod v) = GM.basicInitialize v
  basicUnsafeRead (MV_IntMod v) i = IntMod <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_IntMod v) i (IntMod x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance KnownNat n => G.Vector U.Vector (IntMod n) where
  basicUnsafeFreeze (MV_IntMod v) = V_IntMod <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_IntMod v) = MV_IntMod <$> G.basicUnsafeThaw v
  basicLength (V_IntMod v) = G.basicLength v
  basicUnsafeSlice i l (V_IntMod v) = V_IntMod (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_IntMod v) i = IntMod <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_IntMod mv) (V_IntMod v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}

instance KnownNat n => SemiRing (IntMod n) where
  rZero = 0
  rOne = 1
  (<+>) = (+)
  (<.>) = (*)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# SPECIALIZE instance SemiRing IntMod998 #-}
  {-# SPECIALIZE instance SemiRing IntMod107 #-}
instance KnownNat n => Ring (IntMod n) where
  rNegate = negate
  {-# INLINE rNegate #-}
  {-# SPECIALIZE instance Ring IntMod998 #-}
  {-# SPECIALIZE instance Ring IntMod107 #-}