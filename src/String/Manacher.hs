module String.Manacher where
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

manacher :: (U.Unbox a, Eq a) => U.Vector a -> U.Vector Int
manacher !s
  | U.null s = U.empty
  | otherwise = runST $ do
      let !n = U.length s
      !mrad <- UM.replicate n 0

      let extend !i !k
            | i - (k + 1) >= 0 && i + (k + 1) < n && U.unsafeIndex s (i - (k + 1)) == U.unsafeIndex s (i + (k + 1)) = extend i (k + 1)
            | otherwise = k
      
      _ <- forLoopFold 0 (== n) succ (0, -1) $ \(!i0, r0) !i -> do
        !kInit <- 
          if i > r0
            then return 0
            else do
              !ki' <- UM.unsafeRead mrad (i0 * 2 - i)
              return $! min ki' (r0 - i)
        let !k = extend i kInit
        UM.unsafeWrite mrad i k

        if i + k > r0
          then return (i, i + k)
          else return (i0, r0)
      
      U.unsafeFreeze mrad