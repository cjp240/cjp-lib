{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.BFS where
import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Vector.Unboxed qualified as U

import Common.IxVector
import Data.MutableDeque
import Data.MutableQueue

bfs :: (U.Unbox v, Ix v) => (v, v) -> (v -> U.Vector v) -> Int -> U.Vector v -> IxVect v Int
bfs !bnd !adj !unVis !start = runST $ do
  let !n = rangeSize bnd
  dist <- mivNew bnd unVis
  que <- mqNew (2 * n)
  U.forM_ start $ \ !u -> do
    mivWrite dist u 0
    mqPush que u
  
  let go = mqPop que >>= \case
        Nothing -> return ()
        Just !u -> do
          !nxtD <- succ <$> mivRead dist u
          U.forM_ (adj u) $ \ !v -> do
            !dv <- mivRead dist v
            when (dv == unVis) do
              mivWrite dist v nxtD
              mqPush que v
          go
  
  go
  ivUnsafeFreeze dist
{-# INLINE bfs #-}

bfs01 :: (U.Unbox v, Ix v) => (v, v) -> (v -> U.Vector (v, Int)) -> Int -> U.Vector v -> IxVect v Int
bfs01 !bnd !adj !unVis !start = runST $ do
  let !n = rangeSize bnd
  dist <- mivNew bnd unVis
  deq <- mdNew (2 * n)
  U.forM_ start $ \ !u -> do
    mivWrite dist u 0
    mdPushBack deq u

  let go = mdPopFront deq >>= \case
        Nothing -> return ()
        Just !u -> do
          !du <- mivRead dist u
          U.forM_ (adj u) $ \(!v, !w) -> do
            !dv <- mivRead dist v
            let !nxtD = du + w
            when (dv == unVis || nxtD < dv) do
              mivWrite dist v nxtD
              if w == 0 
                then mdPushFront deq v
                else mdPushBack deq v
          go
  
  go
  ivUnsafeFreeze dist
{-# INLINE bfs01 #-}

-- 最短距離, 場合の数
bfsNum :: (U.Unbox v, Ix v, Num a, U.Unbox a) => (v, v) -> (v -> U.Vector v) -> Int -> U.Vector v -> IxVect v (Int, a)
bfsNum !bnd !adj !unVis !start = runST $ do
  let !n = rangeSize bnd
  distNum <- mivNew bnd (unVis, 0)
  que <- mqNew (2 * n)
  U.forM_ start $ \ !u -> do
    mivWrite distNum u (0, 1)
    mqPush que u
  
  let go = mqPop que >>= \case
        Nothing -> return ()
        Just !u -> do
          (!du, !nu) <- mivRead distNum u
          U.forM_ (adj u) $ \ !v -> do
            (!dv, !nv) <- mivRead distNum v
            case () of
              _ | dv == unVis -> do
                    mivWrite distNum v (du + 1, nu)
                    mqPush que v
                | du + 1 == dv -> do
                    mivWrite distNum v (dv, nv + nu)
                | otherwise -> return ()
          go
  
  go
  ivUnsafeFreeze distNum
{-# INLINE bfsNum #-}

-- 前の 辺, 頂点の情報を adj に乗せる
bfsPrev :: (U.Unbox v, Ix v, U.Unbox e) => (v, v) -> (v -> U.Vector (v, e)) -> Int -> e -> U.Vector v -> IxVect v (Int, e)
bfsPrev !bnd !adj !unVis !unVisE !start = runST $ do
  let !n = rangeSize bnd
  distPrev <- mivNew bnd (unVis, unVisE)
  que <- mqNew n

  U.forM_ start $ \ !u -> do
    mivWrite distPrev u (0, unVisE)
    mqPush que u
  
  let go = mqPop que >>= \case
        Nothing -> return ()
        Just !u -> do
          (!du, _) <- mivRead distPrev u
          let !nxtD = du + 1
          U.forM_ (adj u) $ \(!v, !eInfo) -> do
            (!dv, _) <- mivRead distPrev v
            when (dv == unVis) do
              mivWrite distPrev v (nxtD, eInfo)
              mqPush que v
          go

  go
  ivUnsafeFreeze distPrev
{-# INLINE bfsPrev #-}