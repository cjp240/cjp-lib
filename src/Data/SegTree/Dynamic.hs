{-# LANGUAGE RecordWildCards #-}
module Data.SegTree.Dynamic where
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable qualified as UM

data SegTreeDynamic m a = SegTreeDynamic
  { segdL :: !Int,
    segdR :: !Int,
    segdNodes :: !(MutVar (PrimState m) (UM.MVector (PrimState m) (a, Int, Int))),
    segdNumNodes :: !(MutVar (PrimState m) Int),
    segdOp :: !(a -> a -> a),
    segdUnit :: !a
  }

-- [l, r) をサポート
segdNew :: (PrimMonad m, UM.Unbox a) => Int -> Int -> (a -> a -> a) -> a -> m (SegTreeDynamic m a)
segdNew !l !r !op !sUnit = do
  let !initialSz = 1024
  node <- UM.replicate initialSz (sUnit, -1, -1)
  nodeRef <- newMutVar node
  numNode <- newMutVar 1
  return $ SegTreeDynamic l r nodeRef numNode op sUnit

segdRead :: (PrimMonad m, UM.Unbox a) => SegTreeDynamic m a -> Int -> m a
segdRead SegTreeDynamic{..} !i
  | i < segdL || segdR <= i = error "segdRead : out of bounds"
  | otherwise = do
      nodes <- readMutVar segdNodes
      let go !k !l !r
            | k == -1 = return segdUnit
            | r - l <= 1 = (\(!v, _, _) -> v) <$> UM.unsafeRead nodes k
            | otherwise = do
                (_, !lc, !rc) <- UM.unsafeRead nodes k
                let !mid = l + div (r - l) 2
                if i < mid then go lc l mid 
                else go rc mid r

      go 0 segdL segdR
{-# INLINE segdRead #-}
{-# SPECIALIZE segdRead :: UM.Unbox a => SegTreeDynamic (ST s) a -> Int -> ST s a #-}
{-# SPECIALIZE segdRead :: UM.Unbox a => SegTreeDynamic IO a -> Int -> IO a #-}

segdSet :: (PrimMonad m, UM.Unbox a) => SegTreeDynamic m a -> Int -> a -> m ()
segdSet std@SegTreeDynamic{..} !i !x
  | i < segdL || segdR <= i = error "segdSet : out of bounds"
  | otherwise = go 0 segdL segdR
  where
    go !k !l !r
      | r - l <= 1 = do
          nodes <- readMutVar segdNodes
          (_, !lc, !rc) <- UM.unsafeRead nodes k
          UM.unsafeWrite nodes k (x, lc, rc)
      | otherwise = do
          let !mid = l + div (r - l) 2
          !nxtK <- 
            if i < mid then _segdGetChild std k True
            else _segdGetChild std k False
          
          if i < mid then go nxtK l mid
          else go nxtK mid r

          nodes <- readMutVar segdNodes
          (_, !lc, !rc) <- UM.unsafeRead nodes k
          (!lv, _, _) <- _segdNodeRead nodes lc segdUnit
          (!rv, _, _) <- _segdNodeRead nodes rc segdUnit
          let !res = segdOp lv rv
          UM.unsafeWrite nodes k (res, lc, rc)
{-# INLINE segdSet #-}
{-# SPECIALIZE segdSet :: UM.Unbox a => SegTreeDynamic (ST s) a -> Int -> a -> ST s () #-}
{-# SPECIALIZE segdSet :: UM.Unbox a => SegTreeDynamic IO a -> Int -> a -> IO () #-}

segdModify :: (PrimMonad m, UM.Unbox a) => SegTreeDynamic m a -> (a -> a) -> Int -> m ()
segdModify std@SegTreeDynamic{..} !f !i
  | i < segdL || segdR <= i = error "segdModify : out of bounds"
  | otherwise = go 0 segdL segdR
  where
    go !k !l !r
      | r - l <= 1 = do
          nodes <- readMutVar segdNodes
          (!v, !lc, !rc) <- UM.unsafeRead nodes k
          UM.unsafeWrite nodes k (f v, lc, rc)
      | otherwise = do
          let !mid = l + div (r - l) 2
          !nxtK <- 
            if i < mid then _segdGetChild std k True
            else _segdGetChild std k False
          
          if i < mid then go nxtK l mid
          else go nxtK mid r

          nodes <- readMutVar segdNodes
          (_, !lc, !rc) <- UM.unsafeRead nodes k
          (!lv, _, _) <- _segdNodeRead nodes lc segdUnit
          (!rv, _, _) <- _segdNodeRead nodes rc segdUnit
          let !res = segdOp lv rv
          UM.unsafeWrite nodes k (res, lc, rc)
{-# INLINE segdModify #-}
{-# SPECIALIZE segdModify :: UM.Unbox a => SegTreeDynamic (ST s) a -> (a -> a) -> Int -> ST s () #-}
{-# SPECIALIZE segdModify :: UM.Unbox a => SegTreeDynamic IO a -> (a -> a) -> Int -> IO () #-}

segdProd :: (PrimMonad m, UM.Unbox a) => SegTreeDynamic m a -> Int -> Int -> m a
segdProd SegTreeDynamic{..} !ql !qr
  | segdL > ql || ql > qr || qr > segdR = error "segdProd : out of bounds"
  | ql == qr = return segdUnit
  | otherwise = do
      nodes <- readMutVar segdNodes
      let go !k !l !r
            | k == -1 || qr <= l || r <= ql = return segdUnit
            | ql <= l && r <= qr = (\(!v, _, _) -> v) <$> UM.unsafeRead nodes k
            | otherwise = do
                (_, !lc, !rc) <- UM.unsafeRead nodes k
                let !mid = l + div (r - l) 2
                !lv <- go lc l mid
                !rv <- go rc mid r
                return $! segdOp lv rv      
      go 0 segdL segdR
{-# INLINE segdProd #-}
{-# SPECIALIZE segdProd :: UM.Unbox a => SegTreeDynamic (ST s) a -> Int -> Int -> ST s a #-}
{-# SPECIALIZE segdProd :: UM.Unbox a => SegTreeDynamic IO a -> Int -> Int -> IO a #-}

_segdGetChild :: (PrimMonad m, UM.Unbox a) => SegTreeDynamic m a -> Int -> Bool -> m Int
_segdGetChild SegTreeDynamic{..} !k !isLeft = do
  nodes <- readMutVar segdNodes
  (!v, !l, !r) <- UM.unsafeRead nodes k
  let !child = if isLeft then l else r
  if child /= -1 then return child
  else do
    !newK <- readMutVar segdNumNodes
    let !cap = UM.length nodes
    nodes' <- 
      if newK >= cap then do
        !newNodes <- UM.unsafeGrow nodes cap
        UM.set (UM.unsafeSlice cap cap newNodes) (segdUnit, -1, -1)
        writeMutVar segdNodes newNodes
        return newNodes
      else return nodes
    if isLeft then UM.unsafeWrite nodes' k (v, newK, r)
    else UM.unsafeWrite nodes' k (v, l, newK)
    return newK
{-# INLINE _segdGetChild #-}
{-# SPECIALIZE _segdGetChild :: UM.Unbox a => SegTreeDynamic (ST s) a -> Int -> Bool -> ST s Int #-}
{-# SPECIALIZE _segdGetChild :: UM.Unbox a => SegTreeDynamic IO a -> Int -> Bool -> IO Int #-}

_segdNodeRead :: (PrimMonad m, UM.Unbox a) => UM.MVector (PrimState m) (a, Int, Int) -> Int -> a -> m (a, Int, Int)
_segdNodeRead !nodes !k !sUnit
  | k == -1 = return (sUnit, -1, -1)
  | otherwise = UM.unsafeRead nodes k
{-# INLINE _segdNodeRead #-}
{-# SPECIALIZE _segdNodeRead :: UM.Unbox a => UM.MVector s (a, Int, Int) -> Int -> a -> ST s (a, Int, Int) #-}
{-# SPECIALIZE _segdNodeRead :: UM.Unbox a => UM.MVector RealWorld (a, Int, Int) -> Int -> a -> IO (a, Int, Int) #-}