{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Data.BIT where
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

data BIT m a = BIT
  { bitN :: !Int,
    bitNode :: !(UM.MVector (PrimState m) a),
    bitOp :: !(a -> a -> a),
    bitUnit :: !a
  }

bitNew :: (PrimMonad m, UM.Unbox a) => Int -> (a -> a -> a) -> a -> m (BIT m a)
bitNew n op unit = do
  node <- UM.replicate (n + 1) unit
  return $ BIT n node op unit

bitFromVect :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> U.Vector a -> m (BIT m a)
bitFromVect !op !unit !v = do
  let !n = U.length v
  node <- UM.replicate (n + 1) unit
  U.iforM_ v $ \ !i !x -> do
    UM.unsafeWrite node (i + 1) x
  
  forLoop 1 (== n) succ $ \ !i -> do
    let !j = i + (i .&. (- i))
    when (j <= n) do
      !ni <- UM.unsafeRead node i
      UM.unsafeModify node (\ !nj -> let !nj' = op ni nj in nj') j
  return $ BIT n node op unit

bitFromList :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> [a] -> m (BIT m a)
bitFromList op unit xs = bitFromVect op unit $ U.fromList xs

-- i 番目に x を作用する
bitApply :: (PrimMonad m, UM.Unbox a) => BIT m a -> Int -> a -> m ()
bitApply BIT{..} !i !x = loop (i + 1)
  where
    loop !currI
      | currI <= bitN = do
          UM.unsafeModify bitNode (bitOp x) currI
          let !nxtI = currI + (currI .&. (- currI))
          loop nxtI
      | otherwise = return ()
{-# INLINE bitApply #-}

bitProd :: (PrimMonad m, UM.Unbox a) => BIT m a -> Int -> m a
bitProd BIT{..} !i = loop (i + 1) bitUnit
  where
    loop !j !acc
      | j > 0 = do
          !nj <- UM.unsafeRead bitNode j
          let !j' = j - (j .&. (- j))
              !acc' = bitOp acc nj
          loop j' acc'
      | otherwise = return acc
{-# INLINE bitProd #-}