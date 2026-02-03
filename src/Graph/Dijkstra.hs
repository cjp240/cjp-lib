{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Dijkstra where
import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Vector.Unboxed qualified as U

import Common.IxVector
import Data.MutableHeap

dijkstra :: (Ix v, U.Unbox v) => (v, v) -> (v -> U.Vector (v, Int)) -> U.Vector v -> IxVect v Int
dijkstra !bnd !adj !start = runST $ do
  let !n = rangeSize bnd
  dist <- mivNew bnd (-1)
  hp <- mhNew (n * 2)
  U.forM_ start $ \ !u -> do
    mivWrite dist u 0
    mhPush hp 0 u

  let go = mhPop hp >>= \case
        Nothing -> return ()
        Just (!d, !u) -> do
          !du <- mivRead dist u
          unless (d > du) do
            U.forM_ (adj u) $ \(!v, !w) -> do
              !dv <- mivRead dist v
              let !newD = d + w
              when (dv == -1 || dv > newD) do
                mivWrite dist v newD
                mhPush hp newD v

          go

  go
  ivUnsafeFreeze dist
{-# INLINE dijkstra #-}

dijkstraPrev :: (Ix v, U.Unbox v, U.Unbox e) => (v, v) -> (v -> U.Vector (v, Int, e)) -> e -> U.Vector v -> IxVect v (Int, e)
dijkstraPrev !bnd !adj !unVisE !start = runST $ do
  let !n = rangeSize bnd
  distPrev <- mivNew bnd (-1, unVisE)
  hp <- mhNew (n * 2)
  U.forM_ start $ \ !u -> do
    mivWrite distPrev u (0, unVisE)
    mhPush hp 0 u

  let go = mhPop hp >>= \case
        Nothing -> return ()
        Just (!d, !u) -> do
          (!du, _) <- mivRead distPrev u
          unless (d > du) do
            U.forM_ (adj u) $ \(!v, !w, !eInfo) -> do
              (!dv, _) <- mivRead distPrev v
              let !newD = d + w
              when (dv == -1 || dv > newD) do
                mivWrite distPrev v (newD, eInfo)
                mhPush hp newD v
          go

  go
  ivUnsafeFreeze distPrev
{-# INLINE dijkstraPrev #-}