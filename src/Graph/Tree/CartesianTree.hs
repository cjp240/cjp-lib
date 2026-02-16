{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Tree.CartesianTree where
import Control.Monad
import Control.Monad.ST
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Common.Template
import Data.MutableStack

ctBuild :: (Ord a, U.Unbox a) => U.Vector a -> (U.Vector Int, U.Vector Int, U.Vector Int, Int)
ctBuild !xs
  | U.null xs = (U.empty, U.empty, U.empty, -1)
  | otherwise = runST $ do
      let !n = U.length xs
      lc <- UM.replicate n (-1)
      rc <- UM.replicate n (-1)
      pr <- UM.replicate n (-1)

      stack <- msNew n
      
      forLoop 0 (== n) succ $ \ !i -> do
        let !xi = xs U.! i

            popGreater !lastPop = do
              msTop stack >>= \case
                Just !top | xs U.! top > xi -> do
                  _ <- msPop stack
                  popGreater top
                _ -> return lastPop
        
        !lastPop <- popGreater (-1)
        
        when (lastPop /= -1) do
          UM.unsafeWrite lc i lastPop
          UM.unsafeWrite pr lastPop i

        msTop stack >>= \case
          Just !parent -> do
            UM.unsafeWrite rc parent i
            UM.unsafeWrite pr i parent
          Nothing -> return ()

        msPush stack i

      !v <- readMutVar (msVect stack)
      !root <- UM.unsafeRead v 0
      
      (,,,) <$> U.unsafeFreeze lc <*> U.unsafeFreeze rc <*> U.unsafeFreeze pr <*> pure root