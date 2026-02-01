module Algorithm.Sort where
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

fastSortU :: (UM.Unbox a, Ord a) => U.Vector a -> U.Vector a
fastSortU = fastSortByU compare
{-# INLINE fastSortU #-}

fastSortByU :: (UM.Unbox a) => (a -> a -> Ordering) -> U.Vector a -> U.Vector a
fastSortByU ordering = U.modify (VAI.sortBy ordering)
{-# INLINE fastSortByU #-}

fastSortV :: Ord a => V.Vector a -> V.Vector a
fastSortV = fastSortByV compare
{-# INLINE fastSortV #-}

fastSortByV :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
fastSortByV ordering = V.modify (VAI.sortBy ordering)
{-# INLINE fastSortByV #-}

fastSortLU :: (UM.Unbox a, Ord a) => [a] -> [a]
fastSortLU = U.toList . fastSortU . U.fromList
{-# INLINE fastSortLU #-}

fastSortByLU :: UM.Unbox a => (a -> a -> Ordering) -> [a] -> [a]
fastSortByLU ordering = U.toList . fastSortByU ordering . U.fromList
{-# INLINE fastSortByLU #-}

fastSortLV :: Ord a => [a] -> [a]
fastSortLV = V.toList . fastSortV . V.fromList
{-# INLINE fastSortLV #-}

fastSortByLV :: (a -> a -> Ordering) -> [a] -> [a]
fastSortByLV ordering = V.toList . fastSortByV ordering . V.fromList
{-# INLINE fastSortByLV #-}