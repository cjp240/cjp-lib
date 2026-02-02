{-# LANGUAGE BlockArguments #-}
module Graph.TopSort where
import Control.Monad
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Data.MutableQueue
import Data.MutableHeap
import Graph.CSR

topSort :: Int -> U.Vector (Int, Int) -> Maybe (U.Vector Int)
topSort !n !edges = runST $ do
  let !g = csrBuild n edges

  inDeg <- UM.replicate n (0 :: Int)
  U.forM_ edges $ \ (_, !v) -> UM.unsafeModify inDeg succ v
  
  que <- mqNew n
  res <- UM.new n

  forLoop 0 (== n) succ $ \ !i -> do
    !d <- UM.unsafeRead inDeg i
    when (d == 0) do mqPush que i

  let go !idx = do
        !m <- mqPop que
        case m of
          Nothing -> return idx
          Just !u -> do
            UM.unsafeWrite res idx u
            let !adju = csrAdj g u
            U.forM_ adju $ \ !v -> do
              !d <- UM.unsafeRead inDeg v
              if d == 1 then do
                UM.unsafeWrite inDeg v 0
                mqPush que v
              else UM.unsafeWrite inDeg v $! d - 1
            go (idx + 1)

  !cnt <- go 0
  if cnt == n then Just <$> U.unsafeFreeze res
  else return Nothing
{-# INLINABLE topSort #-}

-- 辞書順最小
topSortMin :: Int -> U.Vector (Int, Int) -> Maybe (U.Vector Int)
topSortMin !n !edges = runST $ do
  let !g = csrBuild n edges

  inDeg <- UM.replicate n (0 :: Int)
  U.forM_ edges $ \ (_, !v) -> UM.unsafeModify inDeg succ v
  
  hp <- mhNew n
  res <- UM.new n

  forLoop 0 (== n) succ $ \ !i -> do
    !d <- UM.unsafeRead inDeg i
    when (d == 0) do mhPush hp i i

  let go !idx = do
        !m <- mhPop hp
        case m of
          Nothing -> return idx
          Just (_, !u) -> do
            UM.unsafeWrite res idx u
            let !adju = csrAdj g u
            U.forM_ adju $ \ !v -> do
              !d <- UM.unsafeRead inDeg v
              if d == 1 then do
                UM.unsafeWrite inDeg v 0
                mhPush hp v v
              else UM.unsafeWrite inDeg v $! d - 1
            go (idx + 1)

  !cnt <- go 0
  if cnt == n then Just <$> U.unsafeFreeze res
  else return Nothing
{-# INLINABLE topSortMin #-}

topSortUnique :: Int -> U.Vector (Int, Int) -> Maybe (Bool, U.Vector Int)
topSortUnique !n !edges = runST $ do
  let !g = csrBuild n edges

  inDeg <- UM.replicate n (0 :: Int)
  U.forM_ edges $ \ (_, !v) -> UM.unsafeModify inDeg succ v
  
  que <- mqNew n
  res <- UM.new n

  forLoop 0 (== n) succ $ \ !i -> do
    !d <- UM.unsafeRead inDeg i
    when (d == 0) do mqPush que i

  let go !idx !isUni = do
        !qSize <- do
          !h <- readMutVar (mqHead que)
          !t <- readMutVar (mqTail que)
          return (t - h)
        
        let !isUni' = isUni && qSize <= 1

        !top <- mqPop que
        case top of
          Nothing -> return (idx, isUni')
          Just !u -> do
            UM.unsafeWrite res idx u
            let !adju = csrAdj g u
            U.forM_ adju $ \ !v -> do
              !d <- UM.unsafeRead inDeg v
              if d == 1 then do
                UM.unsafeWrite inDeg v 0
                mqPush que v
              else UM.unsafeWrite inDeg v $! d - 1
            
            go (idx + 1) isUni'

  (!cnt, !unique) <- go 0 True
  if cnt == n then Just . (unique, ) <$> U.unsafeFreeze res
  else return Nothing
{-# INLINABLE topSortUnique #-}