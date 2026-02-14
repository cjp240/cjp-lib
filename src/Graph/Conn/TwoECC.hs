{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Conn.TwoECC where
import Control.Monad
import Control.Monad.ST
import Data.Bifunctor qualified as BF
import Data.STRef
import Data.Tuple.Extra
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Common.Template
import Data.MutableQueue
import Graph.Conn
import Graph.Conn.LowLink
import Graph.CSR
twoECCDecomp :: LowLink -> (Int, U.Vector Int, U.Vector Int, U.Vector Int)
twoECCDecomp ll@LowLink{..} = runST $ do
  cmp <- UM.replicate llN (-1 :: Int)
  cidRef <- newSTRef 0

  que <- mqNew llN

  let go !cid = mqPop que >>= \case
        Nothing -> mqClear que
        Just !u -> do
          let !vs = csrAdj llCSR u
          U.forM_ vs $ \ (!v, _) -> do
            let !isB = isBridge ll u v
            !cv <- UM.unsafeRead cmp v
            when (not isB && cv == -1) do
              UM.unsafeWrite cmp v cid
              mqPush que v
          go cid

  forLoop 0 (== llN) succ $ \ !v -> do
    !cv <- UM.unsafeRead cmp v
    when (cv == -1) do
      !cid <- readSTRef cidRef
      writeSTRef cidRef $! cid + 1
      mqPush que v
      UM.unsafeWrite cmp v cid
      go cid

  !nc <- readSTRef cidRef
  !fCmp <- U.unsafeFreeze cmp
  let (!vsCmp, !offsetCmp) = csrBuild nc $ U.map swap $ U.indexed fCmp
  return (nc, fCmp, vsCmp, offsetCmp)

twoECCReduct :: LowLink -> (Int, U.Vector Int, U.Vector Int, U.Vector Int, U.Vector (Int, Int))
twoECCReduct ll@LowLink{..} = (nc, cmp, vs, offset, treeEdges)
  where
    (!nc, !cmp, !vs, !offset) = twoECCDecomp ll
    !bridges = getBridges ll
    !treeEdges = U.map (BF.bimap (U.unsafeIndex cmp) (U.unsafeIndex cmp). U.unsafeIndex llEdges) bridges
{-# INLINE twoECCReduct #-}