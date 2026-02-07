module String.ZAlgorithm where
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

zAlgorithm :: (U.Unbox a, Eq a) => U.Vector a -> U.Vector Int
zAlgorithm s
  | U.null s = U.empty
  | otherwise = runST $ do
      let !n = U.length s
      !mz <- UM.replicate n 0
      UM.unsafeWrite mz 0 n

      let extend !i !k
            | i + k < n && U.unsafeIndex s k == U.unsafeIndex s (i + k) = extend i (k + 1)
            | otherwise = k

      _ <- forLoopFold 1 (== n) succ (0, 1) $ \ (!l, !r) !i -> do
        !ziInit <- 
          if i > r
            then return 0
            else do
              !zk <- UM.unsafeRead mz (i - l)
              return $! min (r - i + 1) zk
        let !zi = extend i ziInit
        UM.unsafeWrite mz i zi
        if i + zi - 1 > r
          then return (i, i + zi - 1)
          else return (l, r)
      
      U.unsafeFreeze mz