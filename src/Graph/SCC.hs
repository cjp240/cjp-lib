{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.SCC where
import Control.Monad
import Control.Monad.Extra
import Control.Monad.ST
import Data.Bifunctor qualified as BF
import Data.STRef
import Data.Tuple.Extra
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Algorithm.Sort
import Common.Template
import Data.MutableStack
import Graph.CSR

sccDecomp :: Int -> U.Vector (Int, Int) -> (Int, U.Vector Int, U.Vector Int, U.Vector Int)
sccDecomp !n !edges = runST $ do
  let !g = csrBuild n edges

  ordV <- UM.replicate n (-1 :: Int)
  low <- UM.unsafeNew n
  sccId <- UM.replicate n (-1 :: Int)
  dfsSt <- msNew n
  nodeSt <- msNew n

  step <- newSTRef (0 :: Int)
  cnt <- newSTRef (0 :: Int)

  let dfs !v0 = do
        msPush dfsSt (True, v0, -1)
        let go = msPop dfsSt >>= \case
              Nothing -> return ()
              Just (True, !u, !p) -> do
                -- 行きがけ
                whenM ((== -1) <$> UM.unsafeRead ordV u) do
                  msPush dfsSt (False, u, p)
                  !su <- readSTRef step
                  UM.unsafeWrite ordV u su
                  UM.unsafeWrite low u su
                  writeSTRef step $! su + 1
                  msPush nodeSt u

                  let !adj = csrAdj g u
                  U.forM_ adj $ \ !v -> do
                    !ov <- UM.unsafeRead ordV v
                    if ov == -1 
                      then msPush dfsSt (True, v, u)
                      else do
                        !sIdV <- UM.unsafeRead sccId v
                        when (sIdV == -1) do
                          UM.unsafeModify low (min ov) u
                
                go

              Just (False, !u, !p) -> do
                -- 帰りがけ
                !lu <- UM.unsafeRead low u
                !ou <- UM.unsafeRead ordV u
                when (lu == ou) do
                  !c <- readSTRef cnt
                  let collect = msPop nodeSt >>= \case
                        Nothing -> return ()
                        Just !v -> do
                          UM.unsafeWrite sccId v c
                          unless (v == u) do collect
                  collect
                  writeSTRef cnt $! c + 1

                when (p /= -1) do
                  UM.unsafeModify low (min lu) p
                
                go

        go

  forLoop 0 (== n) succ $ \ !u -> do
    !iu <- UM.unsafeRead sccId u
    when (iu == -1) do dfs u
  
  !numC <- readSTRef cnt  
  !cmp <- U.map (\ !sid -> numC - 1 - sid) <$> U.unsafeFreeze sccId
  let (!vsCmp, !offsetCmp) = csrBuild numC $ U.map swap $ U.indexed cmp
  return (numC, cmp, vsCmp, offsetCmp)

sccReduct :: Int -> U.Vector (Int, Int) -> (Int, U.Vector Int, U.Vector Int, U.Vector Int, U.Vector (Int, Int))
sccReduct !n !edges = (numSCC, cmp, vs, offset, dagEdges)
  where
    (!numSCC, !cmp, !vs, !offset) = sccDecomp n edges
    !dagEdges = U.uniq $ fastSortU $ U.filter (uncurry (/=)) $ U.map (BF.bimap (cmp U.!) (cmp U.!)) edges