{-# LANGUAGE RecordWildCards #-}
module Math.FPS.Sparse.Extra where
import Control.Monad.ST
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Common.Template
import Math.IntMod
import Math.IntMod.Combinatorics
import Math.FPS
import Math.FPS.Sparse

sfpsDiff :: KnownNat n => SparseFPS n -> SparseFPS n
sfpsDiff SparseFPS{..} = SparseFPS $ U.mapMaybe go sfpsVec
  where
    go (!d, !v) = 
      let !v' = v * fromIntegral d
      in
        if v' == 0
          then Nothing
          else Just (d - 1, v')
{-# INLINE sfpsDiff #-}

sfpsIntegral :: KnownNat n => Factorials n -> SparseFPS n -> SparseFPS n
sfpsIntegral !facts SparseFPS{..} = SparseFPS $ U.mapMaybe go sfpsVec
  where
    go (!d, !v) = 
      let !v' = v * modGetInv facts (d + 1)
      in
        if v' == 0
          then Nothing
          else Just (d + 1, v')
{-# INLINE sfpsIntegral #-}

sfpsInv :: KnownNat n => Int -> SparseFPS n -> FPS n
sfpsInv !limit SparseFPS{..}
  | U.null sfpsVec || fst (U.head sfpsVec) /= 0 = error "sfpsInv : head is zero"
  | otherwise = fpsShrink $ FPS $ runST $ do
      g <- UM.replicate limit 0
      let (_, !f0) = U.head sfpsVec
          !invF0 = recip f0
          !fRest = U.tail sfpsVec
          !k = U.length fRest
          done !i !idx = idx == k || fst (U.unsafeIndex fRest idx) > i
      UM.unsafeWrite g 0 invF0
      forLoop 1 (== limit) succ $ \ !i -> do
        !s <- forLoopFold 0 (done i) succ 0 $ \ !acc !idx -> do
          let (!j, !fj) = U.unsafeIndex fRest idx
          !gij <- UM.unsafeRead g $! i - j
          return $! acc + fj * gij
        UM.unsafeWrite g i $! - (s * invF0)
      U.unsafeFreeze g
{-# INLINABLE sfpsInv #-}

sfpsExp :: KnownNat n => Factorials n -> Int -> SparseFPS n -> FPS n
sfpsExp !facts !limit SparseFPS{..}
  | not (U.null sfpsVec) && fst (U.head sfpsVec) == 0 = error "sfpsExp : head not zero"
  | limit <= 0 = 0
  | U.null sfpsVec = 1
  | otherwise = fpsShrink $ FPS $ runST $ do
      g <- UM.replicate limit 0
      UM.unsafeWrite g 0 1
      let !jf = U.map (\(!j, !fj) -> (j, fromIntegral j * fj)) sfpsVec
          !k = U.length jf
          done !i !idx = idx == k || fst (U.unsafeIndex jf idx) > i

      forLoop 1 (== limit) succ $ \ !i -> do
        !s <- forLoopFold 0 (done i) succ 0 $ \ !acc !idx -> do
          let (!j, !jfj) = U.unsafeIndex jf idx
          !gij <- UM.unsafeRead g $! i - j
          return $! acc + jfj * gij
        UM.unsafeWrite g i $! s * modGetInv facts (fromIntegral i)
      U.unsafeFreeze g
{-# INLINABLE sfpsExp #-}

sfpsPow :: forall n k. (KnownNat n, Integral k) => Factorials n -> Int -> k -> SparseFPS n -> FPS n
sfpsPow !facts !limit !k SparseFPS{..}
  | limit <= 0 = 0
  | k == 0 = 1
  | U.null sfpsVec = 0
  | U.head sfpsVec == (0, 1) = fpsShrink $ FPS $ runST $ do
      g <- UM.replicate limit 0
      UM.unsafeWrite g 0 1
      let !m = fromIntegral $ modVal @n
          !kk = fromIntegral $ rem k m
          !fRest = U.tail sfpsVec
          !kn = U.length fRest
          done !i !idx = idx == kn || fst (U.unsafeIndex fRest idx) > i
      
      forLoop 1 (== limit) succ $ \ !i -> do
        !s <- forLoopFold 0 (done i) succ 0 $ \ !acc !idx -> do
          let (!j, !fj) = U.unsafeIndex fRest idx
          !gij <- UM.unsafeRead g $! i - j
          let !term = (fromIntegral j * (kk + 1) - fromIntegral i) * fj * gij
          return $! acc + term
        UM.unsafeWrite g i $! s * modGetInv facts i
      U.unsafeFreeze g
  | otherwise = 
      let (!d, !v) = U.head sfpsVec
          !dk = toInteger d * toInteger k
      in
        if dk >= toInteger limit
          then 0
          else 
            let !limit' = limit - fromIntegral dk
                !invV = recip v
                !vk = modPow v k
                !f' = SparseFPS $ U.map (\(!di, !vi) -> (di - d, vi * invV)) sfpsVec
                FPS !res = sfpsPow facts limit' k f'
            in FPS $ U.replicate (fromIntegral dk) 0 U.++ U.map (* vk) res
{-# INLINABLE sfpsPow #-}

sfpsLog :: KnownNat n => Factorials n -> Int -> SparseFPS n -> FPS n
sfpsLog !facts !limit SparseFPS{..}
  | U.null sfpsVec || U.head sfpsVec /= (0, 1) = error "sfpsLog : head not one"
  | limit <= 0 = 0
  | otherwise = fpsShrink $ FPS $ runST $ do
      g <- UM.replicate limit 0
      let !fRest = U.tail sfpsVec
          !k = U.length fRest
          done !i !idx = idx == k || fst (U.unsafeIndex fRest idx) >= i
          findFi !i !idx
            | idx < k && fst (U.unsafeIndex fRest idx) == i = snd (U.unsafeIndex fRest idx)
            | otherwise = 0
      
          go !i !idx_a
            | i == limit = return ()
            | otherwise = do
                let !fi = findFi i idx_a
                    !ifi = fromIntegral i * fi
                !s <- forLoopFold 0 (done i) succ 0 $ \ !acc !idx -> do
                  let (!j, !fj) = U.unsafeIndex fRest idx
                  !gij <- UM.unsafeRead g (i - j)
                  let !term = fj * fromIntegral (i - j) * gij
                  return $! acc + term
                UM.unsafeWrite g i $! (ifi - s) * modGetInv facts i
                let !nxt_idx_a = 
                      if idx_a < k && fst (U.unsafeIndex fRest idx_a) == i
                        then idx_a + 1
                        else idx_a
                go (i + 1) nxt_idx_a
      go 1 0
      U.unsafeFreeze g
{-# INLINABLE sfpsLog #-}

sfpsSqrt :: KnownNat n => Factorials n -> Int -> SparseFPS n -> Maybe (FPS n)
sfpsSqrt !facts !limit SparseFPS{..}
  | U.null sfpsVec = Just 0
  | otherwise = 
      let (!d, !f0) = U.head sfpsVec
          !d2 = shiftR d 1
      in
        if d2 >= limit then Just 0
        else if odd d then Nothing
        else do
          !s0 <- tonelliShanks f0
          let !s = min s0 (- s0)
              !invF0 = recip f0
              !limit' = limit - d2
              !inv2 = recip 2
              !f' = U.map (\(!di, !vi) -> (di - d, vi * invF0)) sfpsVec
              !fRest = U.tail f'
              !k = U.length fRest
              done !i !idx = idx == k || fst (U.unsafeIndex fRest idx) > i
              !res = runST $ do
                g <- UM.replicate limit' 0
                UM.unsafeWrite g 0 1
                forLoop 1 (== limit') succ $ \ !i -> do
                  !vi <- forLoopFold 0 (done i) succ 0 $ \ !acc !idx -> do
                    let (!j, !fj) = U.unsafeIndex fRest idx
                    !gij <- UM.unsafeRead g $! i - j
                    let !term = (fromIntegral j * (inv2 + 1) - fromIntegral i) * fj * gij
                    return $! acc + term
                  UM.unsafeWrite g i $! vi * modGetInv facts i
                U.unsafeFreeze g
              !res' = U.replicate d2 0 U.++ U.map (* s) res
          return $ fpsShrink $ FPS res'
{-# INLINABLE sfpsSqrt #-}