{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Math.FPS.Poly.Algorithm where
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor qualified as BF
import Data.Bits
import Data.STRef
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.TypeLits

import Common.Template
import Math.IntMod
import Math.FPS
import Math.FPS.Sparse
import Math.FPS.Poly

polyEval :: KnownNat n => Poly n -> IntMod n -> IntMod n
polyEval !p !a = case p of
  S (SparseFPS !s) -> U.sum $ U.map (\(!d, !v) -> v * modPow a d) s
  D (FPS !d) -> U.foldr' (\ !v !acc -> acc * a + v) 0 d
{-# INLINE polyEval #-}

data PolyHeap s n = PolyHeap
  { phVec :: !(VM.MVector s (Int, Poly n)),
    phSize :: !(STRef s Int)
  }

_phNew :: Int -> ST s (PolyHeap s n)
_phNew !cap = do
  vec <- VM.unsafeNew (cap + 1)
  size <- newSTRef 0
  return $ PolyHeap vec size
{-# INLINE _phNew #-}

_phPush :: PolyHeap s n -> Int -> Poly n -> ST s ()
_phPush PolyHeap{..} !p !f = do
  !sz <- readSTRef phSize
  let !sz' = sz + 1
  writeSTRef phSize sz'

  let up 1 = VM.unsafeWrite phVec 1 (p, f)
      up !i = do
        let !parent = shiftR i 1
        (!pp, !pf) <- VM.unsafeRead phVec parent
        if p < pp
          then do
            VM.unsafeWrite phVec i (pp, pf)
            up parent
          else do
            VM.unsafeWrite phVec i (p, f)
  up sz'
{-# INLINE _phPush #-}

_phPop :: PolyHeap s n -> ST s (Maybe (Poly n))
_phPop PolyHeap{..} = do
  !sz <- readSTRef phSize
  if sz == 0
    then return Nothing
    else do
      (_, !res) <- VM.unsafeRead phVec 1
      (!lastP, !lastF) <- VM.unsafeRead phVec sz
      let !sz' = sz - 1
      writeSTRef phSize sz'
      let down !i !tmpP !tmpF = do
            let !l = shiftL i 1
                !r = l .|. 1
            if l > sz'
              then do
                VM.unsafeWrite phVec i (tmpP, tmpF)
              else do
                !childIdx <- 
                  if r <= sz'
                    then do
                      (!lp, _) <- VM.unsafeRead phVec l
                      (!rp, _) <- VM.unsafeRead phVec r
                      if lp < rp
                        then return l
                        else return r
                    else return r
                (!cp, !cf) <- VM.unsafeRead phVec childIdx
                if cp < tmpP
                  then do
                    VM.unsafeWrite phVec i (cp, cf)
                    down childIdx tmpP tmpF
                  else VM.unsafeWrite phVec i (tmpP, tmpF)

      when (sz' > 0) do down 1 lastP lastF
      return $ Just res
{-# INLINE _phPop #-}

polyProduct :: KnownNat n => V.Vector (Poly n) -> Poly n
polyProduct !ps
  | V.null ps = S 1
  | otherwise = runST $ do
      let !n = V.length ps
      case n of
        1 -> return $ V.head ps
        _ -> do
          hp <- _phNew n
          V.forM_ ps $ \ !p -> _phPush hp (_polySize p) p

          forLoop 0 (== n - 1) succ $ \ _ -> do
            !m1 <- _phPop hp
            !m2 <- _phPop hp
            case (m1, m2) of
              (Just !p1, Just !p2) -> do
                let !p12 = p1 * p2
                _phPush hp (_polySize p12) p12
              _ -> error "polyProduct : log error"

          !res <- _phPop hp
          case res of
            Just !p -> return p
            _ -> error "polyProduct : logic error"
{-# INLINABLE polyProduct #-}

polyProductAt :: KnownNat n => Int -> V.Vector (Poly n) -> Poly n
polyProductAt !limit !ps'
  | limit <= 0 = 0
  | V.null ps = 1
  | V.any _polyNull ps = 0
  | otherwise = runST $ do
      let !n = V.length ps
      case n of
        1 -> return $ V.head ps
        _ -> do
          hp <- _phNew n
          V.forM_ ps $ \ !p -> _phPush hp (_polySize p) p
          let go 0 = do
                !res <- _phPop hp
                case res of
                  Just !p -> return p
                  _ -> error "polyProductAt : logic error"
              go !remCnt = do
                !m1 <- _phPop hp
                !m2 <- _phPop hp
                case (m1, m2) of
                  (Just !p1, Just !p2) -> do
                    let !p12 = polyMulAt limit p1 p2
                    if _polyNull p12
                      then return 0
                      else do
                        _phPush hp (_polySize p12) p12
                        go (remCnt - 1)
                  _ -> error "polyProductAt : logic error"
          go (n - 1)
  where
    !ps = V.map (_polyTake limit) ps'
{-# INLINABLE polyProductAt #-}

_polyTake :: KnownNat n => Int -> Poly n -> Poly n
_polyTake !limit !p
  | limit <= 0 = 0
  | otherwise = case p of
      S (SparseFPS !s) -> S $ SparseFPS $ U.takeWhile ((< limit) . fst) s
      D (FPS !d) ->
        let !f = fpsShrink $ FPS $ U.take limit d
        in polyFromDense f
{-# INLINE _polyTake #-}

_polyNull :: KnownNat n => Poly n -> Bool
_polyNull (S (SparseFPS !s)) = U.null s
_polyNull (D (FPS !d)) = U.null d
{-# INLINE _polyNull #-}

_polySize :: KnownNat n => Poly n -> Int
_polySize (S (SparseFPS !s)) = U.length s
_polySize (D (FPS !d)) = U.length d
{-# INLINE _polySize #-}

bostanMori :: (Integral t, Bits t, KnownNat n) => t -> Poly n -> Poly n -> IntMod n
bostanMori !n !p !q = go n p q
  where
    go 0 !a !b = polyCoeffAt 0 a / polyCoeffAt 0 b
    go !k !a !b = 
      let !bNeg = _polyNegateOdd b
          !u = a * bNeg
          !v = b * bNeg
          !a' = _polySample (fromIntegral (k .&. 1)) u
          !b' = _polySample 0 v
      in go (shiftR k 1) a' b'
{-# INLINE bostanMori #-}

_polySample :: KnownNat n => Int -> Poly n -> Poly n
_polySample !offset = \case
  S (SparseFPS !sv) -> polyFromSparse $ SparseFPS $ U.map (BF.first (`shiftR` 1)) $ U.filter ((== offset) . (.&. 1) . fst) sv
  D (FPS !dv) -> polyFromDense $ FPS $ 
    U.generate (shiftR (U.length dv - offset + 1) 1) $ \ !i -> dv U.! (2 * i + offset)
{-# INLINE _polySample #-}

_polyNegateOdd :: KnownNat n => Poly n -> Poly n
_polyNegateOdd = \case
  S (SparseFPS !sv) -> S $ SparseFPS $ U.map (\(!d, !v) -> if odd d then (d, - v) else (d, v)) sv
  D (FPS !dv) -> D $ FPS $ U.imap (\ !i !x -> if odd i then -x else x) dv
{-# INLINE _polyNegateOdd #-}

barlekampMassey :: forall n. KnownNat n => U.Vector (IntMod n) -> (Poly n, Poly n)
barlekampMassey !s = runST $ do
  let !n = U.length s
  c <- UM.replicate (n + 1) 0
  b <- UM.replicate (n + 1) 0
  UM.unsafeWrite c 0 1
  UM.unsafeWrite b 0 1

  lRef <- newSTRef 0
  fRef <- newSTRef 1
  mRef <- newSTRef 1
  lenCRef <- newSTRef 1
  lenBRef <- newSTRef 1

  forLoop 0 (< n) succ $ \ !i -> do
    !l <- readSTRef lRef
    !lenC <- readSTRef lenCRef
    
    !d <- forLoopFold 0 (< lenC) succ 0 $ \ !acc !j -> do
      !cj <- UM.unsafeRead c j
      return $! acc + cj * U.unsafeIndex s (i - j)
    
    if d == 0
      then modifySTRef' mRef succ
      else do
        !f <- readSTRef fRef
        !m <- readSTRef mRef
        !lenB <- readSTRef lenBRef
        let !factor = d / f
        !oldC <- U.freeze (UM.take lenC c)

        forLoop 0 (< lenB) succ $ \ !j -> do
          !bj <- UM.unsafeRead b j
          when (bj /= 0) do
            UM.unsafeModify c (subtract (factor * bj)) (j + m)

        let !newLenC = max lenC (lenB + m)
        writeSTRef lenCRef newLenC

        if shiftL l 1 <= i
          then do
            U.iforM_ oldC $ \ !j !val -> UM.unsafeWrite b j val
            writeSTRef lenBRef lenC
            writeSTRef lRef $! i + 1 - l
            writeSTRef fRef d
            writeSTRef mRef 1
          else modifySTRef' mRef succ
  
  !finalLenC <- readSTRef lenCRef
  !finalL <- readSTRef lRef
  !qVec <- U.unsafeFreeze (UM.take finalLenC c)
  let !q = polyFromDense $ FPS qVec
      !sPoly = polyFromDense $ FPS s
      !p = polyMulAt finalL sPoly q
  
  return (p, q)
{-# INLINABLE barlekampMassey #-}