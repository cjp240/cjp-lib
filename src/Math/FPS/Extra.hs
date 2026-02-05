{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Math.FPS.Extra where

import Control.Monad.ST
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Common.Template
import Math.Convolution.NTT
import Math.FPS
import Math.IntMod
import Math.IntMod.Combinatorics

fpsDiff :: KnownNat n => FPS n -> FPS n
fpsDiff FPS{..}
  | U.length fpsVec <= 1 = FPS U.empty
  | otherwise = fpsShrink $ FPS $ U.imap (\ i x -> x * fromIntegral (i + 1)) $ U.tail fpsVec
{-# INLINE fpsDiff #-}

fpsIntegral :: KnownNat n => Factorials n -> FPS n -> FPS n
fpsIntegral !facts FPS{..}
  | U.null fpsVec = FPS U.empty
  | otherwise = 
      let !len = U.length fpsVec
      in FPS $ U.generate (len + 1) $ \ !i -> 
          if i == 0 
            then 0 
            else modGetInv facts i * U.unsafeIndex fpsVec (i - 1)
{-# INLINE fpsIntegral #-}

fpsInv :: forall n. KnownNat n => Int -> FPS n -> FPS n
fpsInv !limit !f
  | fpsCoeffAt 0 f == 0 = error "fpsInv : head is zero"
  | limit <= 0 = 0
  | otherwise = case _nttCache @n of
      Just !info -> _fpsInvNTT info limit f
      Nothing -> _fpsInvGeneric limit f 
{-# INLINE fpsInv #-}

_fpsInvNTT :: KnownNat n => NTTInfo n -> Int -> FPS n -> FPS n
_fpsInvNTT !info !limit f@FPS{..} = fpsShrink $ runST $ do
  let !bufSize = 2 * limit

  mg <- UM.replicate bufSize 0
  tmpF <- UM.replicate bufSize 0
  tmpG <- UM.replicate bufSize 0
  UM.write mg 0 $! recip $ fpsCoeffAt 0 f

  forLoop 1 (>= limit) (`shiftL` 1) $ \ !m -> do
    let !m2 = 2 * m
        !invM2 = recip $ fromIntegral m2

    -- 1. tmpG = DFT (g, 2m)
    UM.set (UM.unsafeSlice m m tmpG) 0
    UM.unsafeCopy (UM.unsafeTake m tmpG) (UM.unsafeTake m mg)
    _nttButterfly info m2 tmpG

    -- 2. tmpF = DFT (f, 2m)
    let !lenF = min m2 (U.length fpsVec)
    UM.set (UM.unsafeSlice lenF (m2 - lenF) tmpF) 0
    U.unsafeCopy (UM.unsafeTake lenF tmpF) (U.unsafeTake lenF fpsVec)
    _nttButterfly info m2 tmpF

    -- 3. eps = fg - 1
    forLoop 0 (== m2) succ $ \ !i -> do
      !fi <- UM.unsafeRead tmpF i
      !gi <- UM.unsafeRead tmpG i
      UM.unsafeWrite tmpF i $! fi * gi * invM2

    _nttButterflyInv info m2 tmpF
    -- tmpF = fg

    -- [m, 2m) を取り出してeps とし、NTT
    UM.set (UM.unsafeTake m tmpF) 0
    _nttButterfly info m2 tmpF

    -- g * eps
    forLoop 0 (== m2) succ $ \ !i -> do
      !ei <- UM.unsafeRead tmpF i
      !gi <- UM.unsafeRead tmpG i
      UM.unsafeWrite tmpF i $! ei * gi

    _nttButterflyInv info m2 tmpF

    forLoop m (== min m2 limit) succ $ \ !i -> do
      !val <- UM.unsafeRead tmpF i
      UM.unsafeWrite mg i $! - (val * invM2)

  FPS <$> U.unsafeFreeze (UM.unsafeTake limit mg)
{-# INLINABLE _fpsInvNTT #-}

_fpsInvGeneric :: KnownNat n => Int -> FPS n -> FPS n
_fpsInvGeneric !limit !f = 
  let !g0 = FPS $ U.singleton $ recip $ fpsCoeffAt 0 f
      go !m !g
        | m >= limit = fpsShrink $ fpsFixLength limit g
        | otherwise = 
            let !m2 = shiftL m 1
                !fg = fpsMulAt m2 f g
                !res = fpsMulAt m2 g (2 - fg)
            in go m2 res
  in go 1 g0
{-# INLINABLE _fpsInvGeneric #-}

fpsLog :: KnownNat n => Factorials n -> Int -> FPS n -> FPS n
fpsLog !facts !limit !f
  | fpsCoeffAt 0 f /= 1 = error "fpsLog : head not one"
  | limit <= 1 = 0
  | otherwise = 
      let !invF = fpsInv limit f
          !df = fpsDiff f
          !preInteg = fpsMulAt (limit - 1) invF df
      in fpsShrink $ fpsFixLength limit $ fpsIntegral facts preInteg
{-# INLINABLE fpsLog #-}

fpsExp :: forall n. KnownNat n => Factorials n -> Int -> FPS n -> FPS n
fpsExp !facts !limit h@FPS{..}
  | fpsCoeffAt 0 h /= 0 = error "fpsExp : head not zero"
  | limit <= 0 = 0
  | U.null fpsVec = 1
  | limit == 1 = 1
  | otherwise = case _nttCache @n of
      Just !info -> _fpsExpNTT info facts limit h
      Nothing -> _fpsExpGeneric facts limit h
{-# INLINE fpsExp #-}

_fpsExpNTT :: forall n. KnownNat n => NTTInfo n -> Factorials n -> Int -> FPS n -> FPS n
_fpsExpNTT !info !facts !limit h = fpsShrink $ runST $ do
  let !bufSize = 2 * limit
      !dh = fpsDiff h

  mf <- UM.replicate bufSize 0
  mg <- UM.replicate bufSize 0
  tmp1 <- UM.replicate bufSize 0
  tmp2 <- UM.replicate bufSize 0
  dft_f <- UM.replicate bufSize 0
  dft_g <- UM.replicate bufSize 0
  UM.unsafeWrite mf 0 1
  UM.unsafeWrite mg 0 1

  forLoopM 1 (pure . (>= limit)) (pure . (2 *)) $ \ !m -> do
    let !m2 = 2 * m
        !invM2 = modGetInv facts m2
        !invM = modGetInv facts m
    -- 2a. g = (2g - fg^2) mod x^m
    UM.set (UM.unsafeSlice m m dft_f) 0
    UM.unsafeCopy (UM.unsafeTake m dft_f) (UM.unsafeTake m mf)
    _nttButterfly info m2 dft_f

    UM.set (UM.unsafeSlice m m dft_g) 0
    UM.unsafeCopy (UM.unsafeTake m dft_g) (UM.unsafeTake m mg)
    _nttButterfly info m2 dft_g

    forLoop 0 (== m2) succ $ \ !i -> do
      !fi <- UM.unsafeRead dft_f i
      !gi <- UM.unsafeRead dft_g i
      UM.unsafeWrite tmp1 i $! gi * (2 - fi * gi)
    
    _nttButterflyInv info m2 tmp1
    forLoop 0 (== m) succ $ \ !i -> do
      !gi <- UM.unsafeRead tmp1 i
      UM.unsafeWrite mg i $! gi * invM2
    
    -- 2b. q = dh mod x^(m-1)
    UM.set (UM.unsafeTake m tmp1) 0
    let !lendh = min (m - 1) $ U.length (fpsVec dh)
    U.unsafeCopy (UM.unsafeTake lendh tmp1) (U.unsafeTake lendh (fpsVec dh))

    -- 2c. r = fq mod (x^m-1)
    UM.unsafeCopy (UM.unsafeTake m tmp2) (UM.unsafeTake m mf)

    _nttButterfly info m tmp1
    _nttButterfly info m tmp2
    forLoop 0 (== m) succ $ \ !i -> do
      !tmp2i <- UM.unsafeRead tmp2 i
      UM.unsafeModify tmp1 (* (invM * tmp2i)) i
    _nttButterflyInv info m tmp1
    -- tmp1 = r
    -- tmp2 = DFT (f, m)

    -- 2d. s = x(f' - r) mod (x^m-1)
    forLoop 0 (== m) succ $ \ !i -> do
      !fi <- UM.unsafeRead mf i
      !ri_1 <- if i == 0 then UM.unsafeRead tmp1 (m - 1) else UM.unsafeRead tmp1 (i - 1)
      UM.unsafeWrite tmp2 i $! fi * fromIntegral i - ri_1
    -- tmp2 = s

    -- 2e. t = gs mod x^m
    UM.set (UM.unsafeSlice m m tmp2) 0
    _nttButterfly info m2 tmp2 -- tmp2 = DFT (s, 2m)

    UM.set (UM.unsafeSlice m m dft_g) 0
    UM.unsafeCopy (UM.unsafeTake m dft_g) (UM.unsafeTake m mg)
    _nttButterfly info m2 dft_g

    forLoop 0 (== m2) succ $ \ !i -> do
      !si <- UM.unsafeRead tmp2 i
      !gi <- UM.unsafeRead dft_g i
      UM.unsafeWrite tmp1 i $! si * gi

    _nttButterflyInv info m2 tmp1 -- tmp1 = invM2 * t

    -- 2f. u = (h mod x^(2m) - integral (tx^(m-1))) div x^m
    forLoop 0 (== m) succ $ \ !i -> do
      !ti_raw <- UM.unsafeRead tmp1 i
      let !ti = ti_raw * invM2
          !deg = m + i
          !hi = fpsCoeffAt deg h
          !invDeg = modGetInv facts deg
      UM.unsafeWrite tmp2 i $! hi - ti * invDeg
    -- tmp2 = u

    -- 2g. v = fu mod x^m
    UM.set (UM.unsafeSlice m m tmp2) 0
    _nttButterfly info m2 tmp2

    forLoop 0 (== m2) succ $ \ !i -> do
      !ui <- UM.unsafeRead tmp2 i
      !fi <- UM.unsafeRead dft_f i
      UM.unsafeWrite tmp1 i $! ui * fi
    
    _nttButterflyInv info m2 tmp1
    -- tmp1 = v * invM2

    -- 2h. f = f + x^m v
    forLoop 0 (== m) succ $ \ !i -> do
      !vi_raw <- UM.unsafeRead tmp1 i
      let !vi = vi_raw * invM2
      UM.unsafeWrite mf (m + i) vi

  FPS <$> U.unsafeFreeze (UM.unsafeTake limit mf)
{-# INLINABLE _fpsExpNTT #-}

_fpsExpGeneric :: KnownNat n => Factorials n -> Int -> FPS n -> FPS n
_fpsExpGeneric !facts !limit !h = 
  let go !m !g
        | m >= limit = fpsShrink $ fpsFixLength limit g
        | otherwise = 
            let !m2 = shiftL m 1
                !logG = fpsLog facts m2 g
                !res = fpsMulAt m2 g (1 - logG + fpsFixLength m2 h)
            in go m2 res
  in go 1 1
{-# INLINABLE _fpsExpGeneric #-}

fpsPow :: (KnownNat n, Integral k) => Factorials n -> Int -> k -> FPS n -> FPS n
fpsPow !facts !limit !k (FPS !fv)
  | limit <= 0 = 0
  | k == 0 = 1
  | U.null fv = 0
  | otherwise = case U.findIndex (/= 0) fv of
      Nothing -> 0
      Just !d -> 
        let !dk = toInteger d * toInteger k
        in 
          if dk >= toInteger limit
            then 0
            else 
              let !newLimit = limit - fromIntegral dk
                  !f' = FPS $ U.drop d fv
                  !a0 = fpsCoeffAt 0 f'
                  !invA0 = recip a0
                  !g = FPS $ U.map (* invA0) $ fpsVec f'
                  !logG = fpsLog facts newLimit g
                  !scaledLogG = FPS $ U.map (* fromIntegral k) $ fpsVec logG
                  !expLogG = fpsExp facts newLimit scaledLogG
                  !coeff = modPow a0 k
                  !res = FPS $ U.map (* coeff) $ fpsVec expLogG
              in fpsShrink $ FPS $ U.replicate (fromInteger dk) 0 U.++ fpsVec res
{-# INLINABLE fpsPow #-}

fpsSqrt :: forall n. KnownNat n => Int -> FPS n -> Maybe (FPS n)
fpsSqrt !limit f@FPS{..} = case U.findIndex (/= 0) fpsVec of
  Nothing -> Just 0
  Just 0 -> do
    let !f0 = fpsCoeffAt 0 f
    !s0 <- tonelliShanks f0
    return $ fpsShrink $ _fpsSqrt limit f s0
  Just !d -> 
    if shiftR d 1 >= limit then Just 0
    else if odd d then Nothing
    else do
      FPS !v <- fpsSqrt (limit - shiftR d 1) (FPS (U.drop d fpsVec))
      return $ FPS $ U.replicate (shiftR d 1) 0 U.++ v
{-# INLINE fpsSqrt #-}

_fpsSqrt :: KnownNat n => Int -> FPS n -> IntMod n -> FPS n
_fpsSqrt !limit !f !s0 = 
  let !s = min s0 (- s0)
      !inv2 = recip 2
      !g0 = FPS $ U.singleton s
      !h0 = FPS $ U.singleton $ recip s
  
      go !m !g !h
        | m >= limit = fpsShrink $ fpsFixLength limit g
        | otherwise = 
            let !m2 = shiftL m 1
                !g2 = fpsMulAt m2 g g
                !diff = g2 - fpsFixLength m2 f
                !dG = fpsMulAt m2 diff h
                !nxtG = g - FPS (U.map (* inv2) (fpsVec dG))
                !gh = fpsMulAt m2 nxtG h
                !errH = 1 - gh
                !dH = fpsMulAt m2 h errH
                !nxtH = h + dH                
            in go m2 nxtG nxtH
  in go 1 g0 h0
{-# INLINABLE _fpsSqrt #-}

fpsDivMod :: KnownNat n => FPS n -> FPS n -> (FPS n, FPS n)
fpsDivMod !f' !g'
  | U.null gv = error "fpsDivMod : division by zero"
  | n < m = (0, f)
  | otherwise = 
      let !degQ = n - m
          !fRev = FPS $ U.reverse fv
          !gRev = FPS $ U.reverse gv
          FPS !qvRev = fpsFixLength (degQ + 1) $ fpsMulAt (degQ + 1) fRev $ fpsInv (degQ + 1) gRev
          !q = fpsShrink $ FPS $ U.reverse qvRev
          !r = fpsShrink $ f - g * q
      in (q, r)
  where
    f@(FPS !fv) = fpsShrink f'
    g@(FPS !gv) = fpsShrink g'
    !n = U.length fv
    !m = U.length gv
{-# INLINABLE fpsDivMod #-}