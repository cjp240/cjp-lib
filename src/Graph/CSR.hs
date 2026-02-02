module Graph.CSR where
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

csrBuild :: UM.Unbox a => Int -> U.Vector (Int, a) -> (U.Vector a, U.Vector Int)
csrBuild !n !edges = runST $ do
  deg <- UM.replicate n 0
  U.forM_ edges $ \(!u, _) -> do
    UM.unsafeModify deg succ u
  
  !offset <- U.scanl' (+) 0 <$> U.unsafeFreeze deg
  !currentPos <- U.thaw offset

  let !m = U.length edges
  !g <- UM.unsafeNew m
  U.forM_ edges $ \(!u, !v) -> do
    !p <- UM.unsafeRead currentPos u
    UM.unsafeWrite g p v
    UM.unsafeWrite currentPos u $ p + 1

  !adj <- U.unsafeFreeze g
  return (adj, offset)
{-# INLINE csrBuild #-}

csrAdj :: U.Unbox a => (U.Vector a, U.Vector Int) -> Int -> U.Vector a
csrAdj (!adj, !offset) !v = 
  let !st = offset U.! v
      !en = offset U.! (v + 1)
  in U.unsafeSlice st (en - st) adj
{-# INLINE csrAdj #-}