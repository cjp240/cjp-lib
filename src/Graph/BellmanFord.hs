{-# LANGUAGE LambdaCase #-}
module Graph.BellmanFord where
import Control.Monad.ST
import Data.Ix
import Data.Vector.Unboxed qualified as U

import Common.IxVector

bellmanFord :: (Ix v, U.Unbox v) => (v, v) -> U.Vector (v, v, Int) -> U.Vector v -> Maybe (IxVect v Int)
bellmanFord !bnd !edges !start = runST $ do
  let !n = rangeSize bnd
      !inf = div maxBound 2
  dist <- mivNew bnd inf
  U.forM_ start $ \ !u -> do
    mivWrite dist u 0

  let go !i
        | i == n = return True
        | otherwise = do
            !updated <- 
              U.foldM'
                (\ !acc (!u, !v, !w) -> do
                  !du <- mivRead dist u
                  !dv <- mivRead dist v
                  let !newD = du + w
                  if du /= inf && dv > newD 
                    then do
                      mivWrite dist v newD
                      return True
                    else return acc
                )
                False edges
            if updated
              then go (i + 1)
              else return False
  
  go 0 >>= \case
    True -> return Nothing
    False -> Just <$> ivUnsafeFreeze dist
{-# INLINABLE bellmanFord #-}