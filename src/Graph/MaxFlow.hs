{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Graph.MaxFlow where
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

data MFGraph m = 
  MFGraph
  { mfNV :: !Int,
    mfOffset :: !(U.Vector Int),
    mfEdgeTo :: !(U.Vector Int),
    mfEdgeRev :: !(U.Vector Int),
    mfEdgeCap :: !(UM.MVector (PrimState m) Int),
    mfLevel :: !(UM.MVector (PrimState m) Int),
    mfIter :: !(UM.MVector (PrimState m) Int),
    mfQueue :: !(UM.MVector (PrimState m) Int),
    mfEdgeMapping :: !(U.Vector Int)
    }

mfSolve :: PrimMonad m => MFGraph m -> Int -> Int -> m Int
mfSolve mfg@MFGraph{..} !s !t = do
  let go !total = do
        !hasPath <- mfBFS mfg s t
        if not hasPath then return total
        else do
          U.copy mfIter mfOffset
          let loop !acc = do
                f <- mfDFS mfg s t (maxBound :: Int)
                if f == 0 then return acc
                else loop $ acc + f
          !pushed <- loop 0
          if pushed == 0 then return total
          else go $ total + pushed
  go 0
{-# SPECIALIZE mfSolve :: MFGraph (ST s) -> Int -> Int -> ST s Int #-}
{-# SPECIALIZE mfSolve :: MFGraph IO -> Int -> Int -> IO Int #-}

mfGetFlows :: PrimMonad m => Int -> U.Vector (Int, Int, Int) -> Int -> Int -> m (Int, U.Vector Int)
mfGetFlows !n !edges !s !t = do
  mfg@MFGraph{..} <- mfBuild n edges
  !f <- mfSolve mfg s t
  let !m = U.length edges

  used <- U.iforM edges $ \ !i (_, _, !cap) -> do
    let !j = mfEdgeMapping U.! i
    !cap' <- UM.unsafeRead mfEdgeCap j
    return $ cap - cap'
  
  return (f, used)
{-# INLINE mfGetFlows #-}
{-# SPECIALIZE mfGetFlows :: Int -> U.Vector (Int, Int, Int) -> Int -> Int -> ST s (Int, U.Vector Int) #-}
{-# SPECIALIZE mfGetFlows :: Int -> U.Vector (Int, Int, Int) -> Int -> Int -> IO (Int, U.Vector Int) #-}

mfBuild :: PrimMonad m => Int -> U.Vector (Int, Int, Int) -> m (MFGraph m)
mfBuild !n !edges = do
  let !m = U.length edges
  outDeg <- UM.replicate n (0 :: Int)
  U.forM_ edges $ \(!u, !v, _) -> do
    UM.unsafeModify outDeg succ u
    UM.unsafeModify outDeg succ v
  
  !pos <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
  !currentPos <- U.thaw pos

  let !totalEdges = m * 2
  toVec <- UM.unsafeNew totalEdges
  revVec <- UM.unsafeNew totalEdges
  capVec <- UM.unsafeNew totalEdges
  mapping <- UM.unsafeNew m
  
  U.forM_ (U.indexed edges) $ \ (!i, (!u, !v, !c)) -> do
    !pU <- UM.unsafeRead currentPos u
    !pV <- UM.unsafeRead currentPos v
    UM.unsafeWrite mapping i pU
    UM.unsafeWrite toVec pU v
    UM.unsafeWrite capVec pU c
    UM.unsafeWrite revVec pU pV
    UM.unsafeWrite toVec pV u
    UM.unsafeWrite capVec pV 0
    UM.unsafeWrite revVec pV pU
    UM.unsafeWrite currentPos u $! pU + 1
    UM.unsafeWrite currentPos v $! pV + 1

  MFGraph n pos
    <$> U.unsafeFreeze toVec
    <*> U.unsafeFreeze revVec
    <*> pure capVec
    <*> UM.unsafeNew n
    <*> UM.unsafeNew (n + 1)
    <*> UM.unsafeNew n
    <*> U.unsafeFreeze mapping

mfBFS :: PrimMonad m => MFGraph m -> Int -> Int -> m Bool
mfBFS MFGraph{..} !s !t = do
  UM.set mfLevel (-1)
  UM.unsafeWrite mfLevel s 0
  UM.unsafeWrite mfQueue 0 s
  let go !qh !qt
        | qh == qt = return ()
        | otherwise = do
            !v <- UM.unsafeRead mfQueue qh
            !lv <- UM.unsafeRead mfLevel v
            let !st = mfOffset U.! v
                !en = mfOffset U.! (v + 1)
            
            !qt' <- forLoopFold st (== en) succ qt $ \ !acc !i -> do
              let !u = mfEdgeTo U.! i
              !lu <- UM.unsafeRead mfLevel u
              !cap <- UM.unsafeRead mfEdgeCap i
              if cap > 0 && lu == -1 then do
                UM.unsafeWrite mfLevel u $! lv + 1
                UM.unsafeWrite mfQueue acc u
                return $! acc + 1
              else return acc
            go (qh + 1) qt'
  go 0 1
  !lt <- UM.unsafeRead mfLevel t
  return $! lt /= -1
{-# SPECIALIZE mfBFS :: MFGraph (ST s) -> Int -> Int -> ST s Bool #-}
{-# SPECIALIZE mfBFS :: MFGraph IO -> Int -> Int -> IO Bool #-}

mfDFS :: PrimMonad m => MFGraph m -> Int -> Int -> Int -> m Int
mfDFS mfg@MFGraph{..} !v !t !f
  | v == t = return f
  | otherwise = do
      !lv <- UM.unsafeRead mfLevel v
      !startIx <- UM.unsafeRead mfIter v
      let !en = mfOffset U.! (v + 1)

      let go !i
            | i == en = do
                UM.unsafeWrite mfIter v en
                return 0
            | otherwise = do
                let !u = mfEdgeTo U.! i
                !lu <- UM.unsafeRead mfLevel u
                !cap <- UM.unsafeRead mfEdgeCap i
                if cap > 0 && lu == lv + 1 then do
                  !d <- mfDFS mfg u t (min f cap)
                  if d > 0 then do
                    UM.unsafeWrite mfEdgeCap i (cap - d)
                    let !rev = mfEdgeRev U.! i
                    UM.unsafeModify mfEdgeCap (+ d) rev
                    UM.unsafeWrite mfIter v i
                    return d
                  else go $ i + 1
                else go $ i + 1

      go startIx
{-# SPECIALIZE mfDFS :: MFGraph (ST s) -> Int -> Int -> Int -> ST s Int #-}
{-# SPECIALIZE mfDFS :: MFGraph IO -> Int -> Int -> Int -> IO Int #-}