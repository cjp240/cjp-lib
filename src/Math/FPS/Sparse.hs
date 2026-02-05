{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Math.FPS.Sparse where
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor qualified as BF
import Data.Ord
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Algorithm.BinSearch
import Common.Template
import Math.IntMod
import Math.FPS

newtype SparseFPS n = SparseFPS {sfpsVec :: U.Vector (Int, IntMod n)}
type SparseFPS998 = SparseFPS 998244353
type SparseFPS107 = SparseFPS 1000000007

sfpsFromVect :: KnownNat n => U.Vector (Int, Int) -> SparseFPS n
sfpsFromVect = SparseFPS . U.filter ((/= 0) . snd) . U.map (BF.second fromIntegral)
{-# INLINE sfpsFromVect #-}

sfpsFromList :: KnownNat n => [(Int, Int)] -> SparseFPS n
sfpsFromList = sfpsFromVect . U.fromList
{-# INLINE sfpsFromList #-}

instance KnownNat n => Num (SparseFPS n) where
  (+) = _sfpsAdd
  !f - !g = f + _sfpsNeg g
  (*) = _sfpsMul
  fromInteger !x = 
    let !r = fromInteger x
    in
      if r == 0
        then SparseFPS U.empty
        else SparseFPS $ U.singleton (0, r)
  abs = undefined
  signum = undefined
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE fromInteger #-}

_sfpsAdd :: KnownNat n => SparseFPS n -> SparseFPS n -> SparseFPS n
_sfpsAdd f@(SparseFPS !fv) g@(SparseFPS !gv)
  | U.null fv = g
  | U.null gv = f
  | otherwise = SparseFPS $ runST $ do
      let !lenF = U.length fv
          !lenG = U.length gv
      res <- UM.replicate (lenF + lenG) (0, 0)
      let go !ixF !ixG !ixR
            | ixF == lenF && ixG == lenG = return ixR
            | ixF == lenF = do
                UM.unsafeWrite res ixR $! U.unsafeIndex gv ixG
                go ixF (ixG + 1) (ixR + 1)
            | ixG == lenG = do
                UM.unsafeWrite res ixR $! U.unsafeIndex fv ixF
                go (ixF + 1) ixG (ixR + 1)
            | otherwise = do
                let (!df, !vf) = U.unsafeIndex fv ixF
                    (!dg, !vg) = U.unsafeIndex gv ixG
                case compare df dg of
                  LT -> do
                    UM.unsafeWrite res ixR (df, vf)
                    go (ixF + 1) ixG (ixR + 1)
                  GT -> do
                    UM.unsafeWrite res ixR (dg, vg)
                    go ixF (ixG + 1) (ixR + 1)
                  EQ -> do
                    let !v = vf + vg
                    if v == 0
                      then go (ixF + 1) (ixG + 1) ixR
                      else do
                        UM.unsafeWrite res ixR (df, v)
                        go (ixF + 1) (ixG + 1) (ixR + 1)
      !sz <- go 0 0 0
      U.unsafeFreeze $ UM.unsafeTake sz res
{-# INLINABLE _sfpsAdd #-}

_sfpsNeg :: KnownNat n => SparseFPS n -> SparseFPS n
_sfpsNeg SparseFPS{..} = SparseFPS $ U.map (\(!d, !v) -> (d, -v)) sfpsVec
{-# INLINE _sfpsNeg #-}

_sfpsMul :: KnownNat n => SparseFPS n -> SparseFPS n -> SparseFPS n
_sfpsMul (SparseFPS !fv) (SparseFPS !gv)
  | U.null fv || U.null gv = 0
  | otherwise = 
      let !lenF = U.length fv
          !lenG = U.length gv
          !pairs = runST $ do
            !res <- UM.unsafeNew (lenF * lenG)
            forLoop 0 (== lenF) succ $ \ !i -> do
              let (!df, !vf) = U.unsafeIndex fv i
              forLoop 0 (== lenG) succ $ \ !j -> do
                let (!dg, !vg) = U.unsafeIndex gv j
                    !ixR = i * lenG + j
                UM.unsafeWrite res ixR (df + dg, vf * vg)
            VAI.sortBy (comparing fst) res
            U.unsafeFreeze res
          !multRes = runST $ do
            res <- UM.unsafeNew (lenF * lenG)
            let go !ixP !ixR
                  | ixP == lenF * lenG = return ixR
                  | otherwise = do
                      let (!d, _) = U.unsafeIndex pairs ixP
                          collect !ixP' !acc
                            | ixP' < lenF * lenG && fst (U.unsafeIndex pairs ixP') == d = 
                                collect (ixP' + 1) (acc + snd (U.unsafeIndex pairs ixP'))
                            | otherwise = (ixP', acc)
                      let (!nxtIxP, !sumV) = collect ixP 0
                      if sumV == 0
                        then go nxtIxP ixR
                        else do
                          UM.unsafeWrite res ixR (d, sumV)
                          go nxtIxP (ixR + 1)
            !sz <- go 0 0
            U.unsafeFreeze $ UM.unsafeTake sz res
          
      in SparseFPS multRes
{-# INLINABLE _sfpsMul #-}

sfpsMulAt :: KnownNat n => Int -> SparseFPS n -> SparseFPS n -> SparseFPS n
sfpsMulAt !limit (SparseFPS !fv) (SparseFPS !gv) = 
  let !f' = SparseFPS $ U.takeWhile ((< limit) . fst) fv
      !g' = SparseFPS $ U.takeWhile ((< limit) . fst) gv
      SparseFPS !res = _sfpsMul f' g'
      !res' = U.takeWhile ((< limit) . fst) res
  in SparseFPS res'
{-# INLINABLE sfpsMulAt #-}

sfpsToDenseAt :: KnownNat n => Int -> SparseFPS n -> FPS n
sfpsToDenseAt !limit SparseFPS{..}
  | U.null sfpsVec = 0
  | limit <= 0 = 0
  | otherwise = fpsShrink $ FPS $ runST $ do
      res <- UM.replicate limit 0
      U.forM_ sfpsVec $ \(!d, !v) -> do
        when (d < limit) do UM.unsafeWrite res d v
      U.unsafeFreeze res
{-# INLINE sfpsToDenseAt #-}

sfpsToDense :: KnownNat n => SparseFPS n -> FPS n
sfpsToDense f@SparseFPS{..}
  | U.null sfpsVec = 0
  | otherwise = 
      let (!d, _) = U.last sfpsVec
      in sfpsToDenseAt (d + 1) f
{-# INLINE sfpsToDense #-}

sfpsFromDense :: KnownNat n => FPS n -> SparseFPS n
sfpsFromDense FPS{..} = SparseFPS $ U.filter ((/= 0) . snd) $ U.indexed fpsVec
{-# INLINE sfpsFromDense #-}

sfpsCoeffAt :: KnownNat n => Int -> SparseFPS n -> IntMod n
sfpsCoeffAt !d SparseFPS{..}
  | U.null sfpsVec = 0
  | otherwise = 
      let !n = U.length sfpsVec
          !idx = binSearchMin ((>= d) . fst . U.unsafeIndex sfpsVec) (-1) n
      in
        if idx < n && fst (U.unsafeIndex sfpsVec idx) == d
          then snd $ U.unsafeIndex sfpsVec idx
          else 0
{-# INLINE sfpsCoeffAt #-}

sfpsDeg :: KnownNat n => SparseFPS n -> Int
sfpsDeg SparseFPS{..}
  | U.null sfpsVec = -1
  | otherwise = fst $ U.last sfpsVec
{-# INLINE sfpsDeg #-}