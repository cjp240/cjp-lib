{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Math.Convolution.NTT where
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import GHC.TypeLits

import Common.Template
import Math.IntMod

_nttPrimitiveRoot :: forall n. KnownNat n => Maybe (IntMod n)
_nttPrimitiveRoot = case modVal @n of
  998244353 -> Just 3
  754974721 -> Just 11
  167772161 -> Just 3
  469762049 -> Just 3
  _ -> Nothing
{-# INLINE _nttPrimitiveRoot #-}

_nttInfo :: forall n. KnownNat n => Maybe (NTTInfo n)
_nttInfo = do
  _ <- _nttPrimitiveRoot @n
  return $ _nttPrepare @n
{-# INLINE _nttInfo #-}

class NTTConvolvable n where
  _nttCache :: Maybe (NTTInfo n)
instance KnownNat n => NTTConvolvable n where
  _nttCache = _nttInfo @n

_nttConvolve :: forall n. KnownNat n => U.Vector (IntMod n) -> U.Vector (IntMod n) -> U.Vector (IntMod n)
_nttConvolve !f !g = _nttConvolveAt (U.length f + U.length g - 1) f g
{-# INLINE _nttConvolve #-}

_nttConvolveAt :: forall n. KnownNat n => Int -> U.Vector (IntMod n) -> U.Vector (IntMod n) -> U.Vector (IntMod n)
_nttConvolveAt !limit !f' !g'
  | limit <= 0 || U.null f || U.null g = U.empty
  | minimum [U.length f, U.length g, limit] <= 60 = _nttConvolveAtNaive limit f g
  | otherwise = 
      case _nttCache @n of
        Just _ -> _nttConvolveAtNTT limit f g
        Nothing -> 
          let !fs = U.map unIntMod f
              !gs = U.map unIntMod g
              !res1 = _nttConvolveAt limit (U.map fromIntegral fs) (U.map fromIntegral gs) :: U.Vector IntMod754
              !res2 = _nttConvolveAt limit (U.map fromIntegral fs) (U.map fromIntegral gs) :: U.Vector IntMod167
              !res3 = _nttConvolveAt limit (U.map fromIntegral fs) (U.map fromIntegral gs) :: U.Vector IntMod469
          in _nttShrink $ U.generate limit $ \ !i -> 
              _nttCRT
                (if i < U.length res1 then res1 U.! i else 0)
                (if i < U.length res2 then res2 U.! i else 0)
                (if i < U.length res3 then res3 U.! i else 0)
  where
    !f = _nttShrink $ U.take limit f'
    !g = _nttShrink $ U.take limit g'
{-# SPECIALIZE _nttConvolveAt :: Int -> U.Vector IntMod754 -> U.Vector IntMod754 -> U.Vector IntMod754 #-}
{-# SPECIALIZE _nttConvolveAt :: Int -> U.Vector IntMod167 -> U.Vector IntMod167 -> U.Vector IntMod167 #-}
{-# SPECIALIZE _nttConvolveAt :: Int -> U.Vector IntMod469 -> U.Vector IntMod469 -> U.Vector IntMod469 #-}

_nttCRT :: KnownNat n => IntMod754 -> IntMod167 -> IntMod469 -> IntMod n
_nttCRT !r1 !r2 !r3 = 
  let !mod1 = mod754
      !mod2 = mod167
      !mod3 = mod469
      !m1Invm2 = 95869806
      !m1m2Invm3 = 187290749

      !a1 = unIntMod r1
      !a2 = unIntMod $ (r2 - fromIntegral a1) * m1Invm2
      !a12 = (fromIntegral a1 + fromIntegral a2 * fromIntegral mod1) `rem` fromIntegral mod3 :: Integer
      !a3 = unIntMod $ (r3 - fromIntegral a12) * m1m2Invm3

      !res1 = fromIntegral a1
      !res2 = fromIntegral a2 * fromIntegral mod1
      !res3 = fromIntegral a3 * (fromIntegral mod1 * fromIntegral mod2)

  in res1 + res2 + res3
{-# INLINE _nttCRT #-}

data NTTInfo n =
  NTTInfo {
    nttRoot :: !(U.Vector (IntMod n)),
    nttIRoot :: !(U.Vector (IntMod n)),
    nttRate2 :: !(U.Vector (IntMod n)),
    nttIRate2 :: !(U.Vector (IntMod n)),
    nttRate3 :: !(U.Vector (IntMod n)),
    nttIRate3 :: !(U.Vector (IntMod n))
  }

_nttShrink :: KnownNat n => U.Vector (IntMod n) -> U.Vector (IntMod n)
_nttShrink !v = 
  let !n = U.length v
      go !i
        | i < 0 = -1
        | U.unsafeIndex v i /= 0 = i
        | otherwise = go (i - 1)
      !idx = go (n - 1)
  in U.unsafeTake (idx + 1) v
{-# INLINE _nttShrink #-}

_nttConvolveAtNTT :: forall n. KnownNat n => Int -> U.Vector (IntMod n) -> U.Vector (IntMod n) -> U.Vector (IntMod n)
_nttConvolveAtNTT !limit !f !g = runST $ do
  let !lenF = U.length f
      !lenG = U.length g
      !len = lenF + lenG - 1
      !z = if len <= 1 then (1 :: Int) else shiftL 1 (finiteBitSize (len - 1) - countLeadingZeros (len - 1))
  case _nttCache @n of
    Nothing -> undefined
    Just !info -> do
  
      mf <- UM.unsafeNew z
      let !cf = min z lenF
      U.unsafeCopy (UM.unsafeTake cf mf) (U.unsafeTake cf f)
      UM.set (UM.unsafeDrop cf mf) 0

      mg <- UM.unsafeNew z
      let !cg = min z lenG
      U.unsafeCopy (UM.unsafeTake cg mg) (U.unsafeTake cg g)
      UM.set (UM.unsafeDrop cg mg) 0

      let !invZ = recip $ fromIntegral z
          !resLen = min z limit

      _nttButterfly info z mf
      _nttButterfly info z mg
      forLoop 0 (== z) succ $ \ !i -> do
        !mgi <- UM.unsafeRead mg i
        UM.unsafeModify mf (* (mgi * invZ)) i
      _nttButterflyInv info z mf
      _nttShrink <$> U.unsafeFreeze (UM.unsafeTake resLen mf)

_nttConvolveAtNaive :: KnownNat n => Int -> U.Vector (IntMod n) -> U.Vector (IntMod n) -> U.Vector (IntMod n)
_nttConvolveAtNaive !limit !f !g = runST $ do
  let !lenF = U.length f
      !lenG = U.length g
      !len = min limit (lenF + lenG - 1)
  res <- UM.replicate len 0
  let !limitI = min limit lenF
  forLoop 0 (== limitI) succ $ \ !i -> do
    let !fi = U.unsafeIndex f i
        !limitJ = min lenG (limit - i)
    forLoop 0 (== limitJ) succ $ \ !j -> do
      let !gj = U.unsafeIndex g j
      UM.unsafeModify res (+ (fi * gj)) (i + j)
  _nttShrink <$> U.unsafeFreeze res
{-# INLINE _nttConvolveAtNaive #-}

_nttPrepare :: forall n. KnownNat n => NTTInfo n
_nttPrepare = runST $ do
  let !m = modVal @n
      !rank2 = countTrailingZeros (m - 1)
  case _nttPrimitiveRoot @n of
    Nothing -> undefined
    Just !g -> do
  
      let (!root, !iroot) = runST $ do
            !r <- UM.unsafeNew (rank2 + 1)
            !ir <- UM.unsafeNew (rank2 + 1)
            let !rr2 = modPow g (shiftR (m - 1) rank2)
                !irr2 = recip rr2
            UM.unsafeWrite r rank2 rr2
            UM.unsafeWrite ir rank2 irr2
            let go !i
                  | i < 0 = return ()
                  | otherwise = do
                      !x <- UM.unsafeRead r (i + 1)
                      UM.unsafeWrite r i $! x * x
                      !ix <- UM.unsafeRead ir (i + 1)
                      UM.unsafeWrite ir i $! ix * ix
                      go $ i - 1

            go $ rank2 - 1
            (, ) <$> U.unsafeFreeze r <*> U.unsafeFreeze ir

          (!rate2, !irate2) = runST $ do
            r2 <- UM.unsafeNew (max 0 (rank2 - 1))
            ir2 <- UM.unsafeNew (max 0 (rank2 - 1))
            let go !i !p !ip
                  | i > rank2 - 2 = return ()
                  | otherwise = do
                      UM.unsafeWrite r2 i $! p * U.unsafeIndex root (i + 2)
                      UM.unsafeWrite ir2 i $! ip * U.unsafeIndex iroot (i + 2)
                      go (i + 1) (p * U.unsafeIndex iroot (i + 2)) (ip * U.unsafeIndex root (i + 2))
            go 0 1 1
            (, ) <$> U.unsafeFreeze r2 <*> U.unsafeFreeze ir2

          (!rate3, !irate3) = runST $ do
            r3 <- UM.unsafeNew (max 0 (rank2 - 2))
            ir3 <- UM.unsafeNew (max 0 (rank2 - 2))
            let go !i !p !ip
                  | i > rank2 - 3 = return ()
                  | otherwise = do
                      UM.unsafeWrite r3 i $! p * U.unsafeIndex root (i + 3)
                      UM.unsafeWrite ir3 i $! ip * U.unsafeIndex iroot (i + 3)
                      go (i + 1) (p * U.unsafeIndex iroot (i + 3)) (ip * U.unsafeIndex root (i + 3))
            go 0 1 1
            (, ) <$> U.unsafeFreeze r3 <*> U.unsafeFreeze ir3

      return $ NTTInfo root iroot rate2 irate2 rate3 irate3

_nttButterfly :: forall n m. (PrimMonad m, KnownNat n) => NTTInfo n -> Int -> UM.MVector (PrimState m) (IntMod n) -> m ()
_nttButterfly NTTInfo{..} !n !v = do
  let !h = countTrailingZeros n
  forLoop 0 (>= h) (+ 2) $ \ !len -> do
    if h - len == 1
      then do
        let !p = shiftL 1 (h - len - 1)
            !limit = shiftL 1 len
            nxt (!s, !rot) = 
              if s + 1 == limit then (s + 1, rot)
              else (s + 1, rot * U.unsafeIndex nttRate2 (countTrailingZeros (complement (fromIntegral s :: Word32))))
        forLoop (0, 1) ((== limit) . fst) nxt $ \(!s, !rot) -> do
          let !offset = shiftL s (h - len)
          forLoop 0 (== p) succ $ \ !i -> do
            !l <- UM.unsafeRead v (i + offset)
            !r <- (* rot) <$> UM.unsafeRead v (i + offset + p)
            UM.unsafeWrite v (i + offset)     $! l + r
            UM.unsafeWrite v (i + offset + p) $! l - r
      else do
        let !p = shiftL 1 (h - len - 2)
            !imag = U.unsafeIndex nttRoot 2
            !limit = shiftL 1 len
            nxt (!s, !rot) = 
              if s + 1 == limit
                then (s + 1, rot)
                else (s + 1, rot * U.unsafeIndex nttRate3 (countTrailingZeros (complement (fromIntegral s :: Word32))))
        forLoop (0, 1) ((== limit) . fst) nxt $ \(!s, !rot) -> do
          let !rot2 = rot * rot
              !rot3 = rot2 * rot
              !offset = shiftL s (h - len)
          forLoop 0 (== p) succ $ \ !i -> do
            !a0 <- UM.unsafeRead v (i + offset)
            !a1 <- (* rot)  <$> UM.unsafeRead v (i + offset + p)
            !a2 <- (* rot2) <$> UM.unsafeRead v (i + offset + p * 2)
            !a3 <- (* rot3) <$> UM.unsafeRead v (i + offset + p * 3)
            let !a1na3imag = (a1 - a3) * imag
            UM.unsafeWrite v (i + offset)         $! a0 + a2 + a1 + a3
            UM.unsafeWrite v (i + offset + p)     $! a0 + a2 - (a1 + a3)
            UM.unsafeWrite v (i + offset + p * 2) $! a0 - a2 + a1na3imag
            UM.unsafeWrite v (i + offset + p * 3) $! a0 - a2 - a1na3imag
{-# SPECIALIZE _nttButterfly :: NTTInfo 998244353 -> Int -> UM.MVector s IntMod998 -> ST s () #-}
{-# SPECIALIZE _nttButterfly :: NTTInfo 998244353 -> Int -> UM.IOVector IntMod998 -> IO () #-}

_nttButterflyInv :: forall n m. (PrimMonad m, KnownNat n) => NTTInfo n -> Int -> UM.MVector (PrimState m) (IntMod n) -> m ()
_nttButterflyInv NTTInfo{..} !n !v = do
  let !h = countTrailingZeros n
  forLoop h (<= 0) (subtract 2) $ \ !len -> do
    if len == 1
      then do
        let !p = shiftL 1 (h - 1)
            !limit = shiftL 1 (len - 1)
            nxt (!s, !irot) = 
              if s + 1 == limit 
                then (s + 1, irot)
                else (s + 1, irot * U.unsafeIndex nttIRate2 (countTrailingZeros (complement (fromIntegral s :: Word32))))
        forLoop (0, 1) ((== limit) . fst) nxt $ \(!s, !irot) -> do
          let !offset = shiftL s (h - len + 1)
          forLoop 0 (== p) succ $ \ !i -> do
            !l <- UM.unsafeRead v (i + offset)
            !r <- UM.unsafeRead v (i + offset + p)
            UM.unsafeWrite v (i + offset)     $! l + r
            UM.unsafeWrite v (i + offset + p) $! (l - r) * irot
      else do
        let !p = shiftL (1 :: Int) (h - len)
            !iimag = U.unsafeIndex nttIRoot 2
            !limit = shiftL (1 :: Int) (len - 2)
            nxt (!s, !irot) = 
              if s + 1 == limit 
                then (s + 1, irot)
                else (s + 1, irot * U.unsafeIndex nttIRate3 (countTrailingZeros (complement (fromIntegral s :: Word32))))
        forLoop (0, 1) ((== limit) . fst) nxt $ \(!s, !irot) -> do
          let !irot2 = irot * irot
              !irot3 = irot2 * irot
              !offset = shiftL s (h - len + 2)
          forLoop 0 (== p) succ $ \ !i -> do
            !a0 <- UM.unsafeRead v (i + offset)
            !a1 <- UM.unsafeRead v (i + offset + p)
            !a2 <- UM.unsafeRead v (i + offset + p * 2)
            !a3 <- UM.unsafeRead v (i + offset + p * 3)
            let !a2na3iimag = (a2 - a3) * iimag
            UM.unsafeWrite v (i + offset)         $! a0 + a1 + a2 + a3
            UM.unsafeWrite v (i + offset + p)     $! (a0 - a1 + a2na3iimag) * irot
            UM.unsafeWrite v (i + offset + p * 2) $! (a0 + a1 - a2 - a3) * irot2
            UM.unsafeWrite v (i + offset + p * 3) $! (a0 - a1 - a2na3iimag) * irot3
{-# SPECIALIZE _nttButterflyInv :: NTTInfo 998244353 -> Int -> UM.MVector s IntMod998 -> ST s () #-}
{-# SPECIALIZE _nttButterflyInv :: NTTInfo 998244353 -> Int -> UM.IOVector IntMod998 -> IO () #-}