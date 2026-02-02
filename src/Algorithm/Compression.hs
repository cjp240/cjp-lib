module Algorithm.Compression where
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U

import Algorithm.Sort
import Algorithm.BinSearch

compressionU :: (Ord a, U.Unbox a) => U.Vector a -> (Int, Int -> a, a -> Int, U.Vector a)
compressionU !v = (nx, i2x, x2i, xv)
  where
    !xv = U.uniq $ fastSortU v
    !nx = U.length xv
    !i2x = (xv U.!)
    x2i !x = binSearchMin ((>= x) . (xv U.!)) (-1) nx
{-# INLINE compressionU #-}

compressionV :: Ord a => V.Vector a -> (Int, Int -> a, a -> Int, V.Vector a)
compressionV !v = (nx, i2x, x2i, xv)
  where
    !xv = V.uniq $ fastSortV v
    !nx = V.length xv
    !i2x = (xv V.!)
    x2i !x = binSearchMin ((>= x) . (xv V.!)) (-1) nx
{-# INLINE compressionV #-}