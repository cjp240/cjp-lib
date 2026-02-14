{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Conn.LowLink where
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Data.MutableStack
import Graph.CSR

data LowLink = LowLink
  { llN :: !Int,
    llEdges :: !(U.Vector (Int, Int)),
    llOrd :: !(U.Vector Int),
    llLow :: !(U.Vector Int),
    llParent :: !(U.Vector Int),
    llTreeChildCnt :: !(U.Vector Int),
    llCSR :: !(U.Vector (Int, Int), U.Vector Int)
  }

llBuild :: Int -> U.Vector (Int, Int) -> LowLink
llBuild !n !edges = runST $ do
  let !m = U.length edges
      !es = U.imap (\ !i (!u, !v) -> (u, (v, i))) edges U.++ U.imap (\ !i (!u, !v) -> (v, (u, i))) edges
      !g = csrBuild n es

  ord_ <- UM.replicate n (-1)
  low <- UM.replicate n (-1)
  parent <- UM.replicate n (-1)
  childCnt <- UM.replicate n 0
  cntRef <- newSTRef 0
  stack <- msNew (4 * n + 2 * m + 100)

  let go = msPop stack >>= \case
        Nothing -> return ()
        
        Just (True, !u, !pEdgeId) -> do
          !uOrd <- UM.unsafeRead ord_ u
          when (uOrd == -1) do
            !k <- readSTRef cntRef
            writeSTRef cntRef $! k + 1
            UM.unsafeWrite ord_ u k
            UM.unsafeWrite low u k

            msPush stack (False, u, -1)

            let !vs = csrAdj g u
            U.forM_ vs $ \(!v, !edgeId) -> do
              when (edgeId /= pEdgeId) do
                !vOrd <- UM.unsafeRead ord_ v
                if vOrd /= -1
                  then do
                    UM.unsafeModify low (min vOrd) u
                  else do
                    UM.unsafeWrite parent v u
                    msPush stack (True, v, edgeId)
          go
        
        Just (False, !u, _) -> do
          !p <- UM.unsafeRead parent u
          when (p /= -1) do
            !uLow <- UM.unsafeRead low u
            UM.unsafeModify low (min uLow) p
            UM.unsafeModify childCnt succ p
          go

  forLoop 0 (== n) succ $ \ !v -> do
    !vOrd <- UM.unsafeRead ord_ v
    when (vOrd == -1) do
      msPush stack (True, v, -1)
      go
  
  LowLink n edges
    <$> U.unsafeFreeze ord_
    <*> U.unsafeFreeze low
    <*> U.unsafeFreeze parent
    <*> U.unsafeFreeze childCnt
    <*> pure g