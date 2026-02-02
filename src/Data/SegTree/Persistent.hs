{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.SegTree.Persistent where
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.MutVar
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data SegPNode a = SegPEmpty | SegPLeaf !a | SegPBranch !a !(SegPNode a) !(SegPNode a)

data SegTreePersistent m a = SegTreePersistent
  { segpSize :: !Int,
    segpOp :: !(a -> a -> a),
    segpUnit :: !a,
    segpRoots :: !(MutVar (PrimState m) (VM.MVector (PrimState m) (SegPNode a))), -- 歴代の根
    segpGen :: !(MutVar (PrimState m) Int) -- 最新の世代
  }

segpNew :: PrimMonad m => Int -> (a -> a -> a) -> a -> m (SegTreePersistent m a)
segpNew !n !op !sUnit = do
  let !initialCap = 1024
  roots <- VM.new initialCap
  VM.write roots 0 SegPEmpty
  rootRef <- newMutVar roots
  gen <- newMutVar 0
  return $! SegTreePersistent n op sUnit rootRef gen
{-# INLINABLE segpNew #-}

segpFromVect :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> U.Vector a -> m (SegTreePersistent m a)
segpFromVect !op !sUnit !v
  | U.null v = segpNew 0 op sUnit
  | otherwise = do
  let !n = U.length v
      
      build !l !r
        | l + 1 == r = SegPLeaf (v U.! l)
        | otherwise = 
            let mid = shiftR (l + r) 1
                !ln = build l mid
                !rn = build mid r
                !vl = _segpGetVal sUnit ln
                !vr = _segpGetVal sUnit rn
                !val = op vl vr
            in SegPBranch val ln rn

  let !initialCap = 1024
  roots <- VM.new initialCap
  VM.write roots 0 $! build 0 n
  rootRef <- newMutVar roots
  gen <- newMutVar 0
  return $! SegTreePersistent n op sUnit rootRef gen
{-# INLINABLE segpFromVect #-}

segpFromList :: (PrimMonad m, UM.Unbox a) => (a -> a -> a) -> a -> [a] -> m (SegTreePersistent m a)
segpFromList !op !sUnit !xs = segpFromVect op sUnit $ U.fromList xs

segpRead :: PrimMonad m => SegTreePersistent m a -> Int -> Int -> m a
segpRead SegTreePersistent{..} !g !i = do
  !lastGen <- readMutVar segpGen
  when (g > lastGen) do error "segpRead : no generation"
  when (i < 0 || i >= segpSize) do error "segpRead : out of bounds"
  
  roots <- readMutVar segpRoots
  !root <- VM.read roots g
  let go SegPEmpty _ _ = segpUnit
      go (SegPLeaf !v) _ _ = v
      go (SegPBranch _ !lch !rch) !l !r = 
        let !mid = shiftR (l + r) 1
        in
          if i < mid then go lch l mid 
          else go rch mid r
  return $ go root 0 segpSize
{-# INLINE segpRead #-}
{-# SPECIALIZE segpRead :: SegTreePersistent (ST s) a -> Int -> Int -> ST s a #-}
{-# SPECIALIZE segpRead :: SegTreePersistent IO a -> Int -> Int -> IO a #-}

segpSet :: PrimMonad m => SegTreePersistent m a -> Int -> Int -> a -> m Int
segpSet SegTreePersistent{..} !g !i !x = do
  !lastGen <- readMutVar segpGen
  when (g > lastGen) do error "segpSet : no generation"
  when (i < 0 || i >= segpSize) do error "segpSet : out of bounds"

  roots <- readMutVar segpRoots
  let !cap = VM.length roots
      !newGen = lastGen + 1

  roots' <- 
    if newGen >= cap then do
      newRoots <- VM.unsafeGrow roots cap
      writeMutVar segpRoots newRoots
      return newRoots
    else return roots

  !root <- VM.unsafeRead roots' g
  let !newRoot = _segpUpdateNode segpOp segpUnit i x root 0 segpSize

  VM.unsafeWrite roots' newGen newRoot
  writeMutVar segpGen newGen
  return newGen
{-# INLINE segpSet #-}
{-# SPECIALIZE segpSet :: SegTreePersistent (ST s) a -> Int -> Int -> a -> ST s Int #-}
{-# SPECIALIZE segpSet :: SegTreePersistent IO a -> Int -> Int -> a -> IO Int #-}

segpProd :: PrimMonad m => SegTreePersistent m a -> Int -> Int -> Int -> m a
segpProd SegTreePersistent{..} !g !ql !qr = do
  !lastGen <- readMutVar segpGen
  when (g > lastGen) do error "segpProd : no generation"
  unless (0 <= ql && ql <= qr && qr <= segpSize) do error "segpProd : out of bounds"

  roots <- readMutVar segpRoots
  !root <- VM.read roots g
  let go SegPEmpty _ _ = segpUnit
      go !node !l !r
        | qr <= l || r <= ql = segpUnit
        | ql <= l && r <= qr = _segpGetVal segpUnit node
        | otherwise = 
            let !mid = shiftR (l + r) 1
                (!lch, !rch) = case node of
                  SegPBranch _ !ln !rn -> (ln, rn)
                  _ -> (SegPEmpty, SegPEmpty)
                !vL = go lch l mid
                !vR = go rch mid r
                !val = segpOp vL vR
            in val

  return $! go root 0 segpSize
{-# INLINE segpProd #-}
{-# SPECIALIZE segpProd :: SegTreePersistent (ST s) a -> Int -> Int -> Int -> ST s a #-}
{-# SPECIALIZE segpProd :: SegTreePersistent IO a -> Int -> Int -> Int -> IO a #-}

segpGetGen :: PrimMonad m => SegTreePersistent m a -> m Int
segpGetGen SegTreePersistent{..} = readMutVar segpGen
{-# INLINE segpGetGen #-}

_segpGetVal :: a -> SegPNode a -> a
_segpGetVal _ (SegPLeaf !v) = v
_segpGetVal _ (SegPBranch !v _ _) = v
_segpGetVal sUnit SegPEmpty = sUnit
{-# INLINE _segpGetVal #-}

_segpUpdateNode :: (a -> a -> a) -> a -> Int -> a -> SegPNode a -> Int -> Int -> SegPNode a
_segpUpdateNode !op !sUnit !i !val !node !l !r
  | l + 1 == r = SegPLeaf val
  | otherwise =
      let !mid = shiftR (l + r) 1
          (!oldL, !oldR) = case node of
            SegPBranch _ !ln !rn -> (ln, rn)
            _ -> (SegPEmpty, SegPEmpty)
          (!newL, !newR) = if i < mid
            then (_segpUpdateNode op sUnit i val oldL l mid, oldR)
            else (oldL, _segpUpdateNode op sUnit i val oldR mid r)
          !vL = _segpGetVal sUnit newL
          !vR = _segpGetVal sUnit newR
          !newV = op vL vR
      in SegPBranch newV newL newR
{-# INLINABLE _segpUpdateNode #-}