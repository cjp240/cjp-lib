{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.WaveletMatrix where
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.STRef
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word

import Algorithm.BinSearch
import Common.IxVector
import Common.Template

data WaveletMatrix = WaveletMatrix
  { wmHeight :: !Int, -- bit数
    wmSize :: !Int, -- 配列長
    wmBits :: !(IxVect (Int, Int) Word64), -- 各段のビット列
    wmRankTable :: !(IxVect (Int, Int) Int), -- 高速rank用の累積和テーブル
    wmZeros :: !(U.Vector Int), -- 各段の 0 の個数
    wmSum :: !(IxVect (Int, Int) Int) -- 各段のソート済み累積和
  }

wmBuild :: Int -> U.Vector Int -> WaveletMatrix
wmBuild !maxA !as = runST $ do
  let !n = U.length as
      !height = if maxA == 0 then 1 else finiteBitSize maxA - countLeadingZeros maxA
      !numBlocks = ceildiv n 64

  mBits <- mivNew_ ((0, 0), (height - 1, numBlocks - 1))
  mRankTab <- mivNew ((0, 0), (height, numBlocks)) (0 :: Int)
  mZeros <- UM.new height
  mSums <-  mivNew_ ((0, 0), (height - 1, n))

  currA <- U.thaw as
  nxtA <- UM.new n

  forLoop (height - 1) (< 0) pred $ \ !d -> do
    -- 累積和
    mivWrite mSums (d, 0) (0 :: Int)
    forLoop 0 (== n) succ $ \ !i -> do
      !val <- UM.unsafeRead currA i
      !prev <- mivRead mSums (d, i)
      mivWrite mSums (d, i + 1) $! val + prev

    p0 <- newSTRef 0
    p1 <- newSTRef 0

    -- ビット列の生成, 安定ソート
    let !mask = bit d
    forLoop 0 (== n) succ $ \ !i -> do
      !val <- UM.unsafeRead currA i
      when (val .&. mask == 0) do
        modifySTRef' p1 succ

    !zeros <- readSTRef p1
    UM.unsafeWrite mZeros d zeros

    forLoop 0 (== numBlocks) succ $ \ !b -> do
      let !st = b * 64
          !en = min n (st + 64)
      
      !finalW <- forLoopFold st (== en) succ 0 $ \ !acc !i -> do
        !val <- UM.unsafeRead currA i
        if val .&. mask == 0 
          then do
            !idx <- readSTRef p0
            UM.unsafeWrite nxtA idx val
            modifySTRef' p0 succ
            return acc
          else do
            !idx <- readSTRef p1
            UM.unsafeWrite nxtA idx val
            modifySTRef' p1 succ
            return (acc .|. bit (i - st))

      mivWrite mBits (d, b) finalW

      let !pop = popCount finalW
      !prevRank <- mivRead mRankTab (d, b)
      mivWrite mRankTab (d, b + 1) $! prevRank + pop
    
    UM.unsafeCopy currA nxtA

  WaveletMatrix height n 
    <$> ivUnsafeFreeze mBits 
    <*> ivUnsafeFreeze mRankTab
    <*> U.unsafeFreeze mZeros
    <*> ivUnsafeFreeze mSums

-- i 番目の値
wmAccess :: WaveletMatrix -> Int -> Int
wmAccess wm@WaveletMatrix{..} !i = 
  snd $ U.foldl' transition (i, 0) $ U.generate wmHeight ((wmHeight - 1) - )
  where
    transition (!currI, !acc) !d = 
      let !isOne = _wmTestBit wm d currI
      in 
        if isOne 
          then 
            let !nxtI = (wmZeros U.! d) + _wmRank1 wm d currI
            in (nxtI, setBit acc d)
          else 
            let !nxtI = _wmRank0 wm d currI
            in (nxtI, acc)
{-# INLINE wmAccess #-}

-- [l, r) 内の v の個数
wmFreq :: WaveletMatrix -> Int -> Int -> Int -> Int
wmFreq wm@WaveletMatrix{..} !l !r !v = 
  let (!finalL, !finalR) = U.foldl' transition (l, r) $ U.generate wmHeight ((wmHeight - 1) - )
  in finalR - finalL
  where
    transition (!curL, !curR) !d = 
      let !l0 = _wmRank0 wm d curL
          !r0 = _wmRank0 wm d curR
      in
        if testBit v d 
          then
            let !off = wmZeros U.! d
            in (off + curL - l0, off + curR - r0)
          else (l0, r0)
{-# INLINE wmFreq #-}

-- v の k 番目 (0-indexed) の index
wmSelect :: WaveletMatrix -> Int -> Int -> Maybe Int
wmSelect wm@WaveletMatrix{..} !k !v = 
  let !finalL = U.foldl' transition 0 $ U.generate wmHeight ((wmHeight - 1) - )
      !targetPos = finalL + k
  in
    if k >= wmFreq wm v 0 wmSize 
      then Nothing
      else Just $ U.foldl' backstep targetPos $ U.generate wmHeight id
  where
    transition !curL !d = 
      let !l0 = _wmRank0 wm d curL
      in
        if testBit v d 
          then
            let !off = wmZeros U.! d
            in off + curL - l0
          else l0

    backstep !curI !d = 
      if testBit v d 
        then _wmSelect1 wm d $ curI - wmZeros U.! d
        else _wmSelect0 wm d curI
{-# INLINE wmSelect #-}

-- [l, r) で k 番目 (0-indexed) に小さい値
wmKthSmallest :: WaveletMatrix -> Int -> Int -> Int -> Int
wmKthSmallest wm@WaveletMatrix{..} !l !r !k = 
  snd $ U.foldl' transition ((l, r, k), 0) $ U.generate wmHeight ((wmHeight - 1) - )
  where
    transition ((!cl, !cr, !ck), !acc) !d = 
      let !l0 = _wmRank0 wm d cl
          !r0 = _wmRank0 wm d cr
          !c0 = r0 - l0
      in 
        if ck < c0 
          then ((l0, r0, ck), acc)
          else 
            let !off = wmZeros U.! d
            in ((off + cl - l0, off + cr - r0, ck - c0), setBit acc d)
{-# INLINE wmKthSmallest #-}

-- [l, r) で k番目 (0-indexed) に大きい値
wmKthLargest :: WaveletMatrix -> Int -> Int -> Int -> Int
wmKthLargest !wm !l !r !k = wmKthSmallest wm l r (r - l - 1 - k)
{-# INLINE wmKthLargest #-}

-- [l, r) 内の [lower, upper) の要素の数
wmRangeFreq :: WaveletMatrix -> Int -> Int -> Int -> Int -> Int
wmRangeFreq !wm !l !r !lower !upper = _wmLessThanFreq wm l r upper - _wmLessThanFreq wm l r lower
{-# INLINE wmRangeFreq #-}

_wmLessThanFreq :: WaveletMatrix -> Int -> Int -> Int -> Int
_wmLessThanFreq wm@WaveletMatrix{..} !l !r !v = 
  let (_, _, !res) = U.foldl' transition (l, r, 0) $ U.generate wmHeight ((wmHeight - 1) - )
  in res
  where
    transition (!cl, !cr, !acc) !d =
      let !l0 = _wmRank0 wm d cl
          !r0 = _wmRank0 wm d cr
          !c0 = r0 - l0
      in
        if testBit v d 
          then
            let !off = wmZeros U.! d
            in (off + cl - l0, off + cr - r0, acc + c0)
          else (l0, r0, acc)
{-# INLINE _wmLessThanFreq #-}

-- [l, r) にある [lower, upper) の要素の和
wmRectSum :: WaveletMatrix -> Int -> Int -> Int -> Int -> Int
wmRectSum !wm !l !r !lower !upper = _wmLessThanSum wm l r upper - _wmLessThanSum wm l r lower
{-# INLINE wmRectSum #-}

_wmLessThanSum :: WaveletMatrix -> Int -> Int -> Int -> Int
_wmLessThanSum wm@WaveletMatrix{..} !l !r !v = 
  let (_, _, !res) = U.foldl' transition (l, r, 0) $ U.generate wmHeight ((wmHeight - 1) - )
  in res
  where
    transition (!cl, !cr, !acc) !d =
      let !l0 = _wmRank0 wm d cl
          !r0 = _wmRank0 wm d cr
          !s0 = ivIndex wmSum (d, r0) - ivIndex wmSum (d, l0)
      in
        if testBit v d 
          then
            let !off = wmZeros U.! d
            in (off + cl - l0, off + cr - r0, acc + s0)
          else (l0, r0, acc)
{-# INLINE _wmLessThanSum #-}

-- [l, r) 内の v 未満の最大値
wmLookupLT :: WaveletMatrix -> Int -> Int -> Int -> Maybe Int
wmLookupLT !wm !l !r !v = 
  let !cnt = _wmLessThanFreq wm l r v
  in
    if cnt == 0 
      then Nothing
      else Just $ wmKthSmallest wm l r (cnt - 1)
{-# INLINE wmLookupLT #-}

-- [l, r) 内の v 以下の最大値
wmLookupLE :: WaveletMatrix -> Int -> Int -> Int -> Maybe Int
wmLookupLE !wm !l !r !v = wmLookupLT wm l r (v + 1)
{-# INLINE wmLookupLE #-}

-- [l, r) 内の v より大きい最小値
wmLookupGT :: WaveletMatrix -> Int -> Int -> Int -> Maybe Int
wmLookupGT !wm !l !r !v = 
  let !cntLE = _wmLessThanFreq wm l r (v + 1)
      !total = r - l
  in
    if cntLE == total 
      then Nothing
      else Just $ wmKthSmallest wm l r cntLE
{-# INLINE wmLookupGT #-}

-- [l, r) 内の v 以上の最小値
wmLookupGE :: WaveletMatrix -> Int -> Int -> Int -> Maybe Int
wmLookupGE !wm !l !r !v =
  let !cntLT = _wmLessThanFreq wm l r v
      !total = r - l
  in
    if cntLT == total 
      then Nothing
      else Just $ wmKthSmallest wm l r cntLT
{-# INLINE wmLookupGE #-}

-- d段目のビット列から, k (0-indexed) の 0/1 の位置を返す
_wmSelect0 :: WaveletMatrix -> Int -> Int -> Int
_wmSelect0 WaveletMatrix{..} !d !k =
  let !numBlocks = ceildiv wmSize 64
      !q = max 0 $ binSearchMin (\ !m -> m * 64 - ivIndex wmRankTable (d, m) > k) 0 numBlocks - 1
      !remK = k - (q * 64 - ivIndex wmRankTable (d, q))
      !word = ivIndex wmBits (d, q)
      !r = _wmSelectInWord (complement word) remK
  in q * 64 + r
{-# INLINABLE _wmSelect0 #-}

_wmSelect1 :: WaveletMatrix -> Int -> Int -> Int
_wmSelect1 WaveletMatrix{..} !d !k = 
  let !numBlocks = ceildiv wmSize 64
      !q = max 0 $ binSearchMin (\ !m -> ivIndex wmRankTable (d, m) > k) 0 numBlocks - 1
      !remK = k - ivIndex wmRankTable (d, q)
      !word = ivIndex wmBits (d, q)
      !r = _wmSelectInWord word remK
  in q * 64 + r
{-# INLINABLE _wmSelect1 #-}

-- word内で K + 1 個目のビットが立っている位置
_wmSelectInWord :: Word64 -> Int -> Int
_wmSelectInWord !word !k = go 0 64 k
  where
    go !low !width !target
      | width == 1 = low
      | otherwise = 
          let !half = div width 2
              !mask = shiftL 1 half - 1
              !cnt = popCount (shiftR word low .&. mask)
          in
            if target < cnt 
              then go low half target
              else go (low + half) (width - half) (target - cnt)
{-# INLINE _wmSelectInWord #-}

-- d 段目の [0, i) に 1 が何個あるか
_wmRank1 :: WaveletMatrix -> Int -> Int -> Int
_wmRank1 WaveletMatrix{..} !d !i = 
  let (!q, !r) = quotRem i 64
      !base = ivIndex wmRankTable (d, q)
      !word = if q < ceildiv wmSize 64 then ivIndex wmBits (d, q) else 0
      !mask = bit r - 1
  in base + popCount (word .&. mask)
{-# INLINE _wmRank1 #-}

_wmRank0 :: WaveletMatrix -> Int -> Int -> Int
_wmRank0 !wm !d !i = i - _wmRank1 wm d i
{-# INLINE _wmRank0 #-}

_wmTestBit :: WaveletMatrix -> Int -> Int -> Bool
_wmTestBit WaveletMatrix{..} !d !i = 
  let (!q, !r) = quotRem i 64
      !word = ivIndex wmBits (d, q)
  in testBit word r
{-# INLINE _wmTestBit #-}