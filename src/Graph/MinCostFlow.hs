{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Graph.MinCostFlow where
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Data.MutableHeap

data MCFGraph m = 
  MCFGraph {
    mcfNV :: !Int,
    mcfOffset :: !(U.Vector Int),
    mcfEdgeTo :: !(U.Vector Int),
    mcfEdgeRev :: !(U.Vector Int),
    mcfEdgeCap :: !(UM.MVector (PrimState m) Int),
    mcfEdgeCost :: !(U.Vector Int),
    mcfH :: !(UM.MVector (PrimState m) Int),
    mcfVisit :: !(UM.MVector (PrimState m) Bool),
    mcfDist :: !(UM.MVector (PrimState m) Int),
    mcfPrevV :: !(UM.MVector (PrimState m) Int),
    mcfPrevE :: !(UM.MVector (PrimState m) Int),
    mcfEdgeMapping :: !(U.Vector Int),
    mcfHeap :: !(MutableHeap m Int Int)
  }

mcfGetFlows :: PrimMonad m => Int -> U.Vector (Int, Int, Int, Int) -> Int -> Int -> Int -> m (Int, Int, U.Vector Int)
mcfGetFlows !n !edges !s !t !fLimit = do
  g@MCFGraph{..} <- mcfBuild n edges
  (!totalFlow, !minCost) <- mcfSolve g s t fLimit
  !used <- U.iforM edges $ \ !i (_, _, !cap, _) -> do
    let !j = mcfEdgeMapping U.! i
    !cap' <- UM.unsafeRead mcfEdgeCap j
    return $! cap - cap'
  return (totalFlow, minCost, used)
{-# INLINE mcfGetFlows #-}
{-# SPECIALIZE mcfGetFlows :: Int -> U.Vector (Int, Int, Int, Int) -> Int -> Int -> Int -> ST s (Int, Int, U.Vector Int) #-}
{-# SPECIALIZE mcfGetFlows :: Int -> U.Vector (Int, Int, Int, Int) -> Int -> Int -> Int -> IO (Int, Int, U.Vector Int) #-}

mcfSolve :: PrimMonad m => MCFGraph m -> Int -> Int -> Int -> m (Int, Int)
mcfSolve mcfg@MCFGraph{..} !s !t !flowLimit = do
  let !inf = div maxBound 2

      solve !resFlow !resCost
        | resFlow >= flowLimit = return (resFlow, resCost)
        | otherwise = do
            !hasPath <- mcfDijkstra mcfg s t inf
            if not hasPath then return (resFlow, resCost)
            else do
              !dt <- UM.unsafeRead mcfDist t

              forLoop 0 (== mcfNV) succ $ \ !i -> do
                !di <- UM.unsafeRead mcfDist i
                !vis <- UM.unsafeRead mcfVisit i
                if vis then UM.unsafeModify mcfH (+ di) i
                else UM.unsafeModify mcfH (+ dt) i
              
              let getMaxPush !v !f
                    | v == s = return f
                    | otherwise = do
                        !pe <- UM.unsafeRead mcfPrevE v
                        !cap <- UM.unsafeRead mcfEdgeCap pe
                        !pv <- UM.unsafeRead mcfPrevV v
                        getMaxPush pv (min f cap)
              
              !push <- getMaxPush t (flowLimit - resFlow)

              let updateG !v
                    | v == s = return ()
                    | otherwise = do
                        !pe <- UM.unsafeRead mcfPrevE v
                        let !rev = mcfEdgeRev U.! pe
                        UM.unsafeModify mcfEdgeCap (subtract push) pe
                        UM.unsafeModify mcfEdgeCap (+ push) rev
                        !pv <- UM.unsafeRead mcfPrevV v
                        updateG pv

              updateG t
              !ht' <- UM.unsafeRead mcfH t
              !hs' <- UM.unsafeRead mcfH s
              let !unitCost = ht' - hs'
              solve (resFlow + push) (resCost + push * unitCost)
  
  solve 0 0
{-# SPECIALIZE mcfSolve :: MCFGraph (ST s) -> Int -> Int -> Int -> ST s (Int, Int) #-}
{-# SPECIALIZE mcfSolve :: MCFGraph IO -> Int -> Int -> Int -> IO (Int, Int) #-}

mcfSlope :: PrimMonad m => MCFGraph m -> Int -> Int -> Int -> m (U.Vector (Int, Int))
mcfSlope mcfg@MCFGraph{..} !s !t !flowLimit = do
  let !inf = div maxBound 2
      !maxPoints = U.length mcfEdgeTo + 1
  resVec <- UM.unsafeNew maxPoints
  UM.unsafeWrite resVec 0 (0, 0)

  let loop !vec !resFlow !resCost !ptr !len
        | resFlow >= flowLimit = return (vec, ptr)
        | ptr == len = do
            !vec' <- UM.unsafeGrow vec len
            loop vec' resFlow resCost ptr (len * 2)
        | otherwise = do
            hasPath <- mcfDijkstra mcfg s t inf
            if not hasPath then return (vec, ptr)
            else do
              !dt <- UM.unsafeRead mcfDist t
              
              forLoop 0 (== mcfNV) succ $ \ !i -> do
                !di <- UM.unsafeRead mcfDist i
                !vis <- UM.unsafeRead mcfVisit i
                if vis then UM.unsafeModify mcfH (+ di) i
                else UM.unsafeModify mcfH (+ dt) i

              let getMaxPush !v !f
                    | v == s = return f
                    | otherwise = do
                        !pe <- UM.unsafeRead mcfPrevE v
                        !cap <- UM.unsafeRead mcfEdgeCap pe
                        !pv <- UM.unsafeRead mcfPrevV v
                        getMaxPush pv (min f cap)

              !push <- getMaxPush t (flowLimit - resFlow)

              let updateG !v
                    | v == s = return ()
                    | otherwise = do
                        !pe <- UM.unsafeRead mcfPrevE v
                        let !rev = mcfEdgeRev U.! pe
                        UM.unsafeModify mcfEdgeCap (subtract push) pe
                        UM.unsafeModify mcfEdgeCap (+ push) rev
                        !pv <- UM.unsafeRead mcfPrevV v
                        updateG pv
              updateG t

              !ht' <- UM.unsafeRead mcfH t
              !hs' <- UM.unsafeRead mcfH s
              let !unitCost = ht' - hs'
                  !flow' = resFlow + push
                  !cost' = resCost + push * unitCost
              UM.unsafeWrite vec ptr (flow', cost')
              loop vec flow' cost' (ptr + 1) len

  (!finalVec, !finalSize) <- loop resVec 0 0 1 maxPoints
  U.unsafeFreeze $ UM.unsafeTake finalSize finalVec
{-# SPECIALIZE mcfSlope :: MCFGraph (ST s) -> Int -> Int -> Int -> ST s (U.Vector (Int, Int)) #-}
{-# SPECIALIZE mcfSlope :: MCFGraph IO -> Int -> Int -> Int -> IO (U.Vector (Int, Int)) #-}

mcfBuild :: PrimMonad m => Int -> U.Vector (Int, Int, Int, Int) -> m (MCFGraph m)
mcfBuild !n !edges = do
  let !m = U.length edges
  outDeg <- UM.replicate n (0 :: Int)
  U.forM_ edges $ \(!u, !v, _, _) -> do
    UM.unsafeModify outDeg succ u
    UM.unsafeModify outDeg succ v
  !pos <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
  currentPos <- U.thaw pos
  let !totalEdges = 2 * m
  toVec <- UM.unsafeNew totalEdges
  revVec <- UM.unsafeNew totalEdges
  capVec <- UM.unsafeNew totalEdges
  costVec <- UM.unsafeNew totalEdges
  mapping <- UM.unsafeNew m

  U.forM_ (U.indexed edges) $ \(!i, (!u, !v, !cap, !cost)) -> do
    !pU <- UM.unsafeRead currentPos u
    !pV <- UM.unsafeRead currentPos v
    UM.unsafeWrite mapping i pU

    UM.unsafeWrite toVec pU v
    UM.unsafeWrite revVec pU pV
    UM.unsafeWrite capVec pU cap
    UM.unsafeWrite costVec pU cost

    UM.unsafeWrite toVec pV u
    UM.unsafeWrite revVec pV pU
    UM.unsafeWrite capVec pV 0
    UM.unsafeWrite costVec pV $ - cost

    UM.unsafeWrite currentPos u $! pU + 1
    UM.unsafeWrite currentPos v $! pV + 1
  
  MCFGraph n pos
    <$> U.unsafeFreeze toVec
    <*> U.unsafeFreeze revVec
    <*> pure capVec
    <*> U.unsafeFreeze costVec
    <*> UM.replicate n (0 :: Int)
    <*> UM.unsafeNew n
    <*> UM.unsafeNew n
    <*> UM.unsafeNew n
    <*> UM.unsafeNew n
    <*> U.unsafeFreeze mapping
    <*> mhNew (totalEdges * 2)

mcfDijkstra :: PrimMonad m => MCFGraph m -> Int -> Int -> Int -> m Bool
mcfDijkstra MCFGraph{..} !s !t !inf = do
  UM.set mcfDist inf
  UM.set mcfVisit False
  UM.unsafeWrite mcfDist s 0
  mhClear mcfHeap
  mhPush mcfHeap 0 s
  !pot <- U.unsafeFreeze mcfH

  let go = do
        !top <- mhPop mcfHeap
        case top of
          Nothing -> return ()
          Just (!d, !v) -> do
            !dv <- UM.unsafeRead mcfDist v
            let !hv = pot U.! v
            if d > dv then go
            else do
              UM.unsafeWrite mcfVisit v True
              let !st = mcfOffset U.! v
                  !en = mcfOffset U.! (v + 1)
              
              forLoop st (== en) succ $ \ !i -> do
                !cap <- UM.unsafeRead mcfEdgeCap i
                let !u = mcfEdgeTo U.! i
                    !cost = mcfEdgeCost U.! i
                    !hu = pot U.! u
                when (cap > 0) do
                  !du <- UM.unsafeRead mcfDist u
                  let !du' = d + cost + hv - hu
                  when (du' < du) do
                    UM.unsafeWrite mcfDist u du'
                    UM.unsafeWrite mcfPrevV u v
                    UM.unsafeWrite mcfPrevE u i
                    mhPush mcfHeap du' u
              go

  go
  UM.unsafeRead mcfVisit t
{-# SPECIALIZE mcfDijkstra :: MCFGraph (ST s) -> Int -> Int -> Int -> ST s Bool #-}
{-# SPECIALIZE mcfDijkstra :: MCFGraph IO -> Int -> Int -> Int -> IO Bool #-}