{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Data.UFP where
import Control.Monad.Primitive
import Data.Ix
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data UnionFindPotential m v p = UnionFindPotential
  { ufpBnd   :: !(v, v)
  , ufpVs    :: !(U.Vector v)
  , ufpParent :: !(UM.MVector (PrimState m) Int)
  , ufpDiff   :: !(UM.MVector (PrimState m) p) -- V[v] - V[parent[v]]
  , ufpNV     :: !(UM.MVector (PrimState m) Int)
  , ufpNE     :: !(UM.MVector (PrimState m) Int)
  , ufpNC     :: !(MutVar (PrimState m) Int)
  , ufpOp     :: !(p -> p -> p)
  , ufpInv    :: !(p -> p)
  , ufpUnit   :: !p
  }

ufpNew :: (PrimMonad m, Ix v, UM.Unbox v, UM.Unbox p) 
       => (v, v) -> (p -> p -> p) -> (p -> p) -> p -> m (UnionFindPotential m v p)
ufpNew !bnd !op !inv !unit = do
  let !n = rangeSize bnd
      !vs = U.fromListN n $ range bnd
  parent <- UM.replicate n (-1)
  diff   <- UM.replicate n unit
  nV     <- UM.replicate n 1
  nE     <- UM.replicate n 0
  nC     <- newMutVar n
  return $ UnionFindPotential bnd vs parent diff nV nE nC op inv unit

_ufpRoot :: (PrimMonad m, UM.Unbox p) => UnionFindPotential m v p -> Int -> m (Int, p)
_ufpRoot ufp@UnionFindPotential{..} !ix = do
  !p <- UM.unsafeRead ufpParent ix
  if p < 0
    then return (ix, ufpUnit)
    else do
      (!root, !pPot) <- _ufpRoot ufp p
      !vPot <- UM.unsafeRead ufpDiff ix
      let !newPot = ufpOp pPot vPot
      UM.unsafeWrite ufpParent ix root
      UM.unsafeWrite ufpDiff ix newPot
      return (root, newPot)
{-# INLINE _ufpRoot #-}

-- V[y] - V[x] = w / V[y] = V[x] + w
ufpUnite :: (PrimMonad m, UM.Unbox p, U.Unbox v, Ix v, Eq p) 
         => UnionFindPotential m v p -> v -> v -> p -> m (Maybe v)
ufpUnite ufp@UnionFindPotential{..} !x !y !w = do
  let !ix = index ufpBnd x
      !iy = index ufpBnd y
  (!irx, !px) <- _ufpRoot ufp ix
  (!iry, !py) <- _ufpRoot ufp iy

  if irx /= iry then do
    !vx <- UM.unsafeRead ufpNV irx
    !vy <- UM.unsafeRead ufpNV iry
    !ex <- UM.unsafeRead ufpNE irx
    !ey <- UM.unsafeRead ufpNE iry
    modifyMutVar' ufpNC pred
    
    -- V[ry] - V[rx] = inv(py) + w + px
    let !diffRyRx = ufpOp (ufpOp (ufpInv py) w) px

    if vx > vy then do
      UM.unsafeWrite ufpParent iry irx
      UM.unsafeWrite ufpNV irx $! vx + vy
      UM.unsafeWrite ufpNE irx $! ex + ey + 1
      UM.unsafeWrite ufpDiff iry diffRyRx
      return $ Just $! ufpVs U.! irx
    else do
      UM.unsafeWrite ufpParent irx iry
      UM.unsafeWrite ufpNV iry $! vx + vy
      UM.unsafeWrite ufpNE iry $! ex + ey + 1
      UM.unsafeWrite ufpDiff irx (ufpInv diffRyRx)
      return $ Just $! ufpVs U.! iry
  else do
    if ufpOp py (ufpInv px) == w then do
      UM.unsafeModify ufpNE succ irx
      return $ Just $! ufpVs U.! irx
    else return Nothing
{-# INLINE ufpUnite #-}

-- V[y] - V[x]
ufpGetDiff :: (PrimMonad m, UM.Unbox p, U.Unbox v, Ix v) 
           => UnionFindPotential m v p -> v -> v -> m (Maybe p)
ufpGetDiff ufp@UnionFindPotential{..} !x !y = do
  let !ix = index ufpBnd x
      !iy = index ufpBnd y
  (!irx, !px) <- _ufpRoot ufp ix
  (!iry, !py) <- _ufpRoot ufp iy
  if irx /= iry then return Nothing
  else return $ Just $! ufpOp py (ufpInv px)
{-# INLINE ufpGetDiff #-}

ufpRoot :: (PrimMonad m, UM.Unbox p, U.Unbox v, Ix v) => UnionFindPotential m v p -> v -> m v
ufpRoot ufp@UnionFindPotential{..} !x = do
  (!ir, _) <- _ufpRoot ufp (index ufpBnd x)
  return $! ufpVs U.! ir
{-# INLINE ufpRoot #-}

ufpSize :: (PrimMonad m, Ix v, UM.Unbox p) => UnionFindPotential m v p -> v -> m Int
ufpSize ufp@UnionFindPotential{..} !x = do
  (!ir, _) <- _ufpRoot ufp (index ufpBnd x)
  UM.unsafeRead ufpNV ir
{-# INLINE ufpSize #-}

ufpNumEdge :: (PrimMonad m, Ix v, UM.Unbox p) => UnionFindPotential m v p -> v -> m Int
ufpNumEdge ufp@UnionFindPotential{..} !x = do
  (!ir, _) <- _ufpRoot ufp (index ufpBnd x)
  UM.unsafeRead ufpNE ir
{-# INLINE ufpNumEdge #-}

ufpNumComponent :: (PrimMonad m) => UnionFindPotential m v p -> m Int
ufpNumComponent UnionFindPotential{..} = readMutVar ufpNC
{-# INLINE ufpNumComponent #-}