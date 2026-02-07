module Common.Template where
import Data.Foldable qualified as F
import Data.Hashable qualified as Hashable
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as U

--------------------------------------------------------------------------------
-- Template : accumMap
--------------------------------------------------------------------------------
accumMap :: (Foldable t, Ord k) => (a -> b -> a) -> a -> Map.Map k a -> t (k, b) -> Map.Map k a
accumMap op def = F.foldl' f
  where
    f m (k, v) = Map.alter (\x -> Just (op (fromMaybe def x) v)) k m
{-# INLINE accumMap #-}

accumIntMap :: Foldable t => (a -> b -> a) -> a -> IM.IntMap a -> t (Int, b) -> IM.IntMap a
accumIntMap op def = F.foldl' f
  where
    f m (k, v) = IM.alter (\x -> Just (op (fromMaybe def x) v)) k m
{-# INLINE accumIntMap #-}

accumHashMap :: (Foldable t, Hashable.Hashable k) => (a -> b -> a) -> a -> HM.HashMap k a -> t (k, b) -> HM.HashMap k a
accumHashMap op def = F.foldl' f
  where
    f m (k, v) = HM.alter (\x -> Just (op (fromMaybe def x) v)) k m
{-# INLINE accumHashMap #-}

--------------------------------------------------------------------------------
-- Template : forLoop
--------------------------------------------------------------------------------
forLoop :: Monad m => s -> (s -> Bool) -> (s -> s) -> (s -> m ()) -> m ()
forLoop !initS !end !next !action = go initS
  where
    go !s | end s = return ()
          | otherwise = do
              action s
              go $ next s
{-# INLINE forLoop #-}

forLoopM :: Monad m => s -> (s -> m Bool) -> (s -> m s) -> (s -> m ()) -> m ()
forLoopM !initS !end !next !action = go initS
  where
    go !s = do
      !done <- end s
      if done then return ()
      else do
        action s
        !nxtS <- next s
        go nxtS
{-# INLINE forLoopM #-}

forLoopFold :: Monad m => s -> (s -> Bool) -> (s -> s) -> a -> (a -> s -> m a) -> m a
forLoopFold !initS !end !next !acc !action = go acc initS
  where
    go !a !s | end s = return a
             | otherwise = do
                !nxtA <- action a s
                let !nxtS = next s
                go nxtA nxtS
{-# INLINE forLoopFold #-}

forLoopFoldM :: Monad m => s -> (s -> m Bool) -> (s -> m s) -> a -> (a -> s -> m a) -> m a
forLoopFoldM !initS !end !next !acc !action = go acc initS
  where
    go !a !s = do
      !done <- end s
      if done then return a
      else do
        !nxtA <- action a s
        !nxtS <- next s
        go nxtA nxtS
{-# INLINE forLoopFoldM #-}

--------------------------------------------------------------------------------
-- Template : util
--------------------------------------------------------------------------------
indexed :: Int -> [a] -> [(Int, a)]
indexed i = zip [i ..]
{-# INLINE indexed #-}

ceildiv :: Integral a => a -> a -> a
ceildiv a b = (a + b - 1) `div` b
{-# INLINE ceildiv #-}

counts :: Ord a => [a] -> [(a, Int)]
counts = runLength . L.sort

add2 :: (Int,Int) -> (Int,Int) -> (Int,Int)
add2 (!x1, !y1) (!x2, !y2) = (x1+x2, y1+y2)
{-# INLINE add2 #-}

nubOrd :: (U.Unbox a, Ord a) => [a] -> [a]
nubOrd = U.toList . U.uniq . U.modify VAI.sort . U.fromList
{-# INLINE nubOrd #-}

minmax :: Ord a => a -> a -> (a, a)
minmax !a !b = (min a b, max a b)
{-# INLINE minmax #-}

maximumDef :: (Foldable t, Ord p) => p -> t p -> p
maximumDef def xs
  | null xs = def
  | otherwise = maximum xs
{-# INLINE maximumDef #-}

minimumDef :: (Foldable t, Ord p) => p -> t p -> p
minimumDef def xs
  | null xs = def
  | otherwise = minimum xs
{-# INLINE minimumDef #-}

runLength :: (Eq a) => [a] -> [(a, Int)]
runLength = map f . L.group
  where
    f xs@(!x : _) = let !len = length xs in (x, len)
    f [] = undefined
{-# INLINE runLength #-}