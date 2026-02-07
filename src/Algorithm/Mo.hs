module Algorithm.Mo where
import Control.Monad.Primitive
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Algorithm.Sort

-- Block size hint: max 1 $ floor (fromIntegral n / sqrt (fromIntegral nq))

moSolve :: (PrimMonad m, UM.Unbox a) => 
  Int -> -- クエリ数
  Int -> -- ブロックサイズ
  (Int -> Int -> m ()) -> 
  (Int -> Int -> m ()) -> 
  (Int -> Int -> m ()) -> 
  (Int -> Int -> m ()) -> 
  m a -> -- 値を得る
  U.Vector (Int, Int) -> -- クエリ
  (Int, Int) -> -- 初期値
  m (U.Vector a)
moSolve !nq !sz !lInc !lDec !rInc !rDec !getans !qs !initQ = do
  let !sortedQs = _moSort sz qs
  ans <- UM.new nq

  _ <- forLoopFold 0 (== nq) succ initQ $ \ !currLR !i -> do
    let (!idx, !nextLR) = sortedQs U.! i
    _moMove lInc lDec rInc rDec currLR nextLR
    !res <- getans
    UM.write ans idx res
    return nextLR

  U.freeze ans
{-# INLINABLE moSolve #-}

_moSort :: 
  Int -> -- ブロックのサイズ
  U.Vector (Int, Int) -> -- クエリ [l, r)
  U.Vector (Int, (Int, Int)) -- (oridignal index, (l, r))
_moSort !sz !qs = U.map (\(_, !l, !r, !i) -> (i, (l, r))) $! fastSortByU cmp $! U.map (\ (!i, (!l, !r)) -> (quot l sz, l, r, i)) $ U.indexed qs
  where
    cmp (!q1, _, !r1, _) (!q2, _, !r2, _)
      | q1 /= q2 = compare q1 q2
      | odd q1 = compare r2 r1
      | otherwise = compare r1 r2

_moMove :: Monad m => 
  (Int -> Int -> m ()) -> -- [l, r) -> [l + 1, r)
  (Int -> Int -> m ()) -> -- [l, r) -> [l - 1, r)
  (Int -> Int -> m ()) -> -- [l, r) -> [l, r + 1)
  (Int -> Int -> m ()) -> -- [l, r) -> [l, r - 1)
  (Int, Int) -> -- 前の区間
  (Int, Int) -> -- 後の区間
  m ()
_moMove lInc lDec rInc rDec (!l1, !r1) (!l2, !r2)
  | l1 <= l2 && r1 <= r2 = do
      forLoop r1 (== r2) succ $ \ !r -> do
        rInc l1 r
      forLoop l1 (== l2) succ $ \ !l -> do
        lInc l r2
  | l1 <= l2 && r1 > r2 = do
      forLoop r1 (== r2) pred $ \ !r -> do
        rDec l1 r
      forLoop l1 (== l2) succ $ \ !l -> do
        lInc l r2
  | l1 > l2 && r1 <= r2 = do
      forLoop r1 (== r2) succ $ \ !r -> do
        rInc l1 r
      forLoop l1 (== l2) pred $ \ !l -> do
        lDec l r2
  | otherwise = do
      forLoop l1 (== l2) pred $ \ !l -> do
        lDec l r1
      forLoop r1 (== r2) pred $ \ !r -> do
        rDec l2 r
{-# INLINE _moMove #-}