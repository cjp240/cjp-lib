{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Graph.BipartiteMatching where
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Data.MutableQueue
import Graph.CSR

data BMGraph =
  BMGraph {
    bmSizeL :: !Int,
    bmSizeR :: !Int,
    bmTo :: !(U.Vector Int),
    bmOffset :: !(U.Vector Int)
  }

buildGraphHK :: PrimMonad m => Int -> Int -> U.Vector (Int, Int) -> m BMGraph
buildGraphHK sizeU sizeV edges = do
  let (!to, !offset) = csrBuild sizeU edges
  return $ BMGraph sizeU sizeV to offset

bmSolve :: BMGraph -> (Int, U.Vector Int)
bmSolve BMGraph{..} = runST $ do
  pairL <- UM.replicate bmSizeL (-1 :: Int)
  pairR <- UM.replicate bmSizeR (-1 :: Int)
  dist <- UM.replicate bmSizeL (-1 :: Int)
  ptr <- UM.replicate bmSizeL (0 :: Int)
  que <- mqNew bmSizeL

  let bfs = do
        UM.set dist (-1)
        writeMutVar (mqHead que) 0
        writeMutVar (mqTail que) 0

        forLoop 0 (== bmSizeL) succ $ \ !l -> do
          !r <- UM.unsafeRead pairL l
          when (r == -1) do
            UM.unsafeWrite dist l 0
            mqPush que l

        let go !foundFreeR = do
              !top <- mqPop que
              case top of
                Nothing -> return foundFreeR
                Just !l -> do
                  !dl <- UM.unsafeRead dist l
                  let !st = bmOffset U.! l
                      !en = bmOffset U.! (l + 1)

                  newFound <- forLoopFold st (== en) succ False $ \ !found !i -> do
                    let !r = bmTo U.! i
                    !nxtL <- UM.unsafeRead pairR r
                    if nxtL == -1 then return True
                    else do
                      !dn <- UM.unsafeRead dist nxtL
                      if dn == -1 then do
                        UM.unsafeWrite dist nxtL $! dl + 1
                        mqPush que nxtL
                        return found
                      else return found

                  go newFound
        
        go False

  let dfs !l = do
        !dl <- UM.unsafeRead dist l
        let !st = bmOffset U.! l
            !en = bmOffset U.! (l + 1)

            go !i
              | i == en = return False
              | otherwise = do
                  let !r = bmTo U.! i
                  !nxtL <- UM.unsafeRead pairR r

                  !canStep <- 
                    if nxtL == -1 then return True
                    else do
                      !dn <- UM.unsafeRead dist nxtL
                      return (dn == dl + 1)

                  if canStep then do
                    !res <- 
                      if nxtL == -1 then return True
                      else dfs nxtL
                    if res then do
                      UM.unsafeWrite pairL l r
                      UM.unsafeWrite pairR r l
                      return True
                    else do
                      UM.unsafeModify ptr succ l
                      go (i + 1)
                  else do
                    UM.unsafeModify ptr succ l
                    go (i + 1)
        
        !currPtr <- UM.unsafeRead ptr l
        go (st + currPtr)

  let mainLoop !acc = do
        !hasPath <- bfs
        if not hasPath then return acc
        else do
          UM.set ptr 0
          !added <- forLoopFold 0 (== bmSizeL) succ 0 $ \ !cnt !l -> do
            !matchR <- UM.unsafeRead pairL l
            if matchR == -1 then do
              !res <- dfs l
              return $ cnt + if res then 1 else 0
            else return cnt

          if added == 0 then return acc
          else mainLoop (acc + added)
  
  !numPairs <- mainLoop 0
  (numPairs, ) <$> U.unsafeFreeze pairL