{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Tree.EulerTour where
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Graph.CSR
import Data.MutableStack

data EulerTourTree = EulerTourTree
  { ettNV :: !Int,
    ettRoot :: !Int,
    ettVisit :: !(U.Vector Int),
    ettVCost1 :: !(U.Vector Int),
    ettVCost2 :: !(U.Vector Int),
    ettECost1 :: !(U.Vector Int),
    ettECost2 :: !(U.Vector Int),
    ettDepth :: !(U.Vector Int),
    ettNStep :: !Int,
    ettVDepth :: !(U.Vector Int),
    ettDiscovery :: !(U.Vector Int),
    ettFinishing :: !(U.Vector Int)
  }deriving Show

ettBuild :: Int -> U.Vector (Int, Int, Int) -> (Int -> Int) -> Int -> EulerTourTree
ettBuild !n !edges !vCost !root = runST $ do
  let !es = U.map (\(!u, !v, !d) -> (u, (v, d))) edges U.++ U.map (\(!u, !v, !d) -> (v, (u, d))) edges
      !g = csrBuild n es
      !nstep = 2 * n
  visit <- UM.replicate nstep 0
  vCost1 <- UM.replicate nstep 0
  vCost2 <- UM.replicate nstep 0
  eCost1 <- UM.replicate nstep 0
  eCost2 <- UM.replicate nstep 0
  depth <- UM.replicate nstep 0
  vDepth <- UM.unsafeNew n
  discovery <- UM.replicate n (-1)
  finishing <- UM.unsafeNew n

  step <- newSTRef 0
  stack <- msNew (4 * n + 100)

  msPush stack (root, -1, 0, 0, True)

  let go = msPop stack >>= \case
        Nothing -> return ()
        Just (!v, !from, !d, !w, True) -> do
          msPush stack (from, v, d - 1, w, False)
          !s <- readSTRef step
          writeSTRef step $! s + 1
          UM.unsafeWrite discovery v s
          UM.unsafeWrite vDepth v d
          UM.unsafeWrite visit s v
          UM.unsafeWrite depth s d
          UM.unsafeWrite vCost1 s $! vCost v
          UM.unsafeWrite vCost2 s $! vCost v
          UM.unsafeWrite eCost1 s w
          UM.unsafeWrite eCost2 s w
          
          let !us = csrAdj g v
          U.forM_ us $ \ (!u, !w') -> do
            when (u /= from) do
              msPush stack (u, v, d + 1, w', True)
          go
        Just (!v, !from, !d, !w, False) -> do
          !s <- readSTRef step
          writeSTRef step $! s + 1
          UM.unsafeWrite finishing from s
          UM.unsafeWrite vCost2 s $! - (vCost from)
          when (v /= -1) do
            UM.unsafeWrite visit s v
            UM.unsafeWrite depth s d
            UM.unsafeWrite eCost2 s $! - w
          go
  
  go

  EulerTourTree n root
    <$> U.unsafeFreeze visit
    <*> U.unsafeFreeze vCost1
    <*> U.unsafeFreeze vCost2
    <*> U.unsafeFreeze eCost1
    <*> U.unsafeFreeze eCost2
    <*> U.unsafeFreeze depth
    <*> pure nstep
    <*> U.unsafeFreeze vDepth
    <*> U.unsafeFreeze discovery
    <*> U.unsafeFreeze finishing