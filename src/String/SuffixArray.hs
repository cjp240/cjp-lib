{-# LANGUAGE BlockArguments #-}
module String.SuffixArray where
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

suffixArray :: Int -> U.Vector Int -> U.Vector Int
suffixArray !numAlphabet !s
  | U.null s = U.singleton 0
  | otherwise = _suffixArray numAlphabet s
{-# INLINE suffixArray #-}

lcpArray :: U.Vector Int -> U.Vector Int -> U.Vector Int
lcpArray !sa !s = runST $ do
  let !n = U.length s
      !t = U.snoc (U.map succ s) 0

  -- rank[sa[i]] = i
  mRank <- UM.unsafeNew (n + 1)
  U.iforM_ sa $ \ !i !pos -> UM.unsafeWrite mRank pos i
  !rank <- U.unsafeFreeze mRank

  lcp <- UM.unsafeNew n

  let headLoop !i !j !h
        | i + h < n + 1 && j + h < n + 1 && t U.! (i + h) == t U.! (j + h) = headLoop i j (h + 1)
        | otherwise = h

  _ <- forLoopFold 0 (== n) succ 0 $ \ !h !i -> do
    let !r = rank U.! i
    if r == 0 
      then return 0 -- sa[0] : 空文字列
      else do
        let !prevIdx = sa U.! (r - 1)
            !h' = headLoop i prevIdx h
        UM.unsafeWrite lcp (r - 1) h'
        return $! max 0 (h' - 1)

  U.cons 0 <$> U.unsafeFreeze lcp
{-# INLINABLE lcpArray #-}

suffixStructure :: Int -> U.Vector Int -> (U.Vector Int, U.Vector Int)
suffixStructure !numAlphabet !s = 
  let !sa = suffixArray numAlphabet s
      !lcp = lcpArray sa s
  in (sa, lcp)
{-# INLINE suffixStructure #-}

_suffixArray :: Int -> U.Vector Int -> U.Vector Int
_suffixArray !numAlphabet !s = runST $ do
  let !t = U.snoc (U.map succ s) 0 -- 番兵
      !n = U.length s
  sa <- UM.replicate (n + 1) (-1 :: Int)

  -- バケットの初めと終わりを計算
  cnt <- UM.replicate (numAlphabet + 1) (0 :: Int)
  U.forM_ t $ \ !c -> UM.unsafeModify cnt succ c
  !offset <- U.scanl' (+) 0 <$> U.unsafeFreeze cnt
  sts0 <- U.thaw $ U.unsafeInit offset
  ens0 <- U.thaw $ U.unsafeTail offset
  -- iから始まるバケット : [st[i], en[i])

  -- typeの分類 (0 : s-type, 1 : l-type), lms の詰め込み
  slType <- UM.unsafeNew (n + 1)
  UM.unsafeWrite slType n (0 :: Int)
  lms <- UM.unsafeNew (n + 1) -- lms の 元の配列でのindex

  !nlms <- forLoopFold (n - 1) (< 0) pred 0 $ \ !acc !i -> do
    let !c = t U.! i
        !c' = t U.! (i + 1)
    !ti' <- UM.unsafeRead slType (i + 1)
    let !ti | c < c' = 0
            | c > c' = 1
            | otherwise = ti'
    UM.unsafeWrite slType i ti

    -- s[i + 1] : LMS
    if ti == 1 && ti' == 0 
      then do
        !enc' <- UM.unsafeRead ens0 c'
        UM.unsafeWrite sa (enc' - 1) $! i + 1
        UM.unsafeWrite ens0 c' $! enc' - 1
        UM.unsafeWrite lms acc $! i + 1
        return $! acc + 1
      else return acc

  -- lmsの昇順
  !lmsOrderd <- U.reverse <$> U.unsafeFreeze (UM.unsafeTake nlms lms)
  mlmsIndices <- UM.replicate (n + 1) (-1)
  U.iforM_ lmsOrderd $ \ !i !l -> UM.unsafeWrite mlmsIndices l i
  -- s[i] が何番目の lms か
  !lmsIndices <- U.unsafeFreeze mlmsIndices
  let isLMS = (/= -1) . (lmsIndices U.!)
  
      -- i番目のlmsSubstring
      lmsSubString !i
        | i == -1 = U.empty
        | i == nlms - 1 = 
            let !li = lmsOrderd U.! i
            in U.unsafeSlice li (n - li + 1) t
        | otherwise = 
            let !li = lmsOrderd U.! i
                !li' = lmsOrderd U.! (i + 1)
            in U.unsafeSlice li (li' - li + 1) t

  -- induced sort
  ens1 <- U.thaw $ U.unsafeTail offset
  _saInducedSort n t slType sa sts0 ens1

  -- lmsは, lms-subtring が昇順になる順番で出てくる
  -- lmsSubString に index を付ける
  lmsSubIndices <- UM.unsafeNew nlms

  -- (直前のlmsが何番目か, 直前のlmsSubに付けたindexが何か)
  (_, !maxSubIdx) <- forLoopFold 0 (> n) succ (-1, -1 :: Int) $ \ (!prevLMS, !prevIdx) !i -> do
    !v <- UM.unsafeRead sa i
    if v /= -1 && isLMS v 
      then do
        let !currLMS = lmsIndices U.! v
            !prevLMSSub = lmsSubString prevLMS
            !currLMSSub = lmsSubString currLMS
        if prevLMSSub < currLMSSub 
          then do
            UM.unsafeWrite lmsSubIndices currLMS $! prevIdx + 1
            return (currLMS, prevIdx + 1)
          else do
            UM.unsafeWrite lmsSubIndices currLMS prevIdx
            return (currLMS, prevIdx)
      else return (prevLMS, prevIdx)

  !lmsSubString' <- U.unsafeFreeze lmsSubIndices

  !saLMS <- 
    if maxSubIdx + 1 == nlms -- すべてユニークな順位
      then do
        !res <- UM.unsafeNew nlms
        U.iforM_ lmsSubString' $ \ !i !subIdx -> UM.unsafeWrite res subIdx i
        U.unsafeFreeze res
      else return $! U.tail $ _suffixArray (maxSubIdx + 1) lmsSubString' -- 重複があるので、SAで求める

  UM.set sa (-1)

  ens2 <- U.thaw $ U.unsafeTail offset
  forLoop (nlms - 1) (< 0) pred $ \ !i -> do
    let !lmsIdx = saLMS U.! i
        !originalPos = lmsOrderd U.! lmsIdx
        !c = t U.! originalPos
    !enc <- UM.unsafeRead ens2 c
    UM.unsafeWrite sa (enc - 1) originalPos
    UM.unsafeWrite ens2 c (enc - 1)

  sts3 <- U.thaw $ U.unsafeInit offset
  ens3 <- U.thaw $ U.unsafeTail offset
  _saInducedSort n t slType sa sts3 ens3

  U.unsafeFreeze sa
{-# INLINABLE _suffixArray #-}

_saInducedSort :: Int -> U.Vector Int -> UM.MVector s Int -> UM.MVector s Int -> UM.MVector s Int -> UM.MVector s Int -> ST s ()
_saInducedSort !n !t !slType !sa !sts !ens = do
  forLoop 0 (> n) succ $ \ !i -> do
    !si <- UM.unsafeRead sa i
    when (si > 0) do
      !ti' <- UM.unsafeRead slType (si - 1)
      when (ti' == 1) do
        let !c = t U.! (si - 1)
        !stc <- UM.unsafeRead sts c
        UM.unsafeWrite sa stc $! si - 1
        UM.unsafeWrite sts c $! stc + 1

  forLoop n (< 0) pred $ \ !i -> do
    !si <- UM.unsafeRead sa i
    when (si > 0) do
      !ti' <- UM.unsafeRead slType (si - 1)
      when (ti' == 0) do
        let !c = t U.! (si - 1)
        !enc <- UM.unsafeRead ens c
        UM.unsafeWrite sa (enc - 1) $! si - 1
        UM.unsafeWrite ens c $! enc - 1
{-# INLINE _saInducedSort #-}