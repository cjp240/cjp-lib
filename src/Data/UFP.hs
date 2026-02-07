{-# LANGUAGE RecordWildCards #-}
module Data.UFP where
import Control.Monad
import Control.Monad.Primitive
import Data.Ix
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Data.UF

data UnionFindPotential m v p = UnionFindPotential
  { ufpUF :: !(UnionFind m v),
    ufpDiff :: !(UM.MVector (PrimState m) p), -- V[v] - V[parent [v]]
    ufpOp :: !(p -> p -> p),
    ufpInv :: !(p -> p),
    ufpUnit :: !p
  }

ufpNew :: (PrimMonad m, UM.Unbox v, UM.Unbox p, Ix v) => (v, v) -> (p -> p -> p) -> (p -> p) -> p -> m (UnionFindPotential m v p)
ufpNew !bnd !op !inv !unit = do
  uf <- ufNew bnd
  let !n = rangeSize bnd
  diff <- UM.replicate n unit
  return $ UnionFindPotential uf diff op inv unit

ufpRoot :: (PrimMonad m, UM.Unbox p, U.Unbox v, Ix v) => UnionFindPotential m v p -> v -> m (v, p)
ufpRoot ufp@UnionFindPotential{..} !x = do
  let !ix = index (ufBnd ufpUF) x
  (!ir, !p) <- _ufpRoot ufp ix
  return (ufVs ufpUF U.! ir, p)
{-# INLINE ufpRoot #-}

-- V[y] - V[x]
ufpGetDiff :: (PrimMonad m, UM.Unbox p, Ix v) => UnionFindPotential m v p -> v -> v -> m (Maybe p)
ufpGetDiff ufp@UnionFindPotential{..} !x !y = do
  let !bnd = ufBnd ufpUF
      !ix = index bnd x
      !iy = index bnd y
  (!irx, !px) <- _ufpRoot ufp ix
  (!iry, !py) <- _ufpRoot ufp iy
  if irx /= iry then return Nothing
  else return $ Just $! ufpOp py $ ufpInv px
{-# INLINE ufpGetDiff #-}

_ufpRoot :: (PrimMonad m, UM.Unbox p) => UnionFindPotential m v p -> Int -> m (Int, p)
_ufpRoot ufp@UnionFindPotential{..} !ix = do
  !p <- UM.unsafeRead (ufParent ufpUF) ix
  if p == -1 then return (ix, ufpUnit)
  else do
    (!root, !diffRootP) <- _ufpRoot ufp p
    !diffPV <- UM.unsafeRead ufpDiff ix
    let !diffRootV = ufpOp diffRootP diffPV
    UM.unsafeWrite (ufParent ufpUF) ix root
    UM.unsafeWrite ufpDiff ix diffRootV
    return (root, diffRootV)
{-# INLINE _ufpRoot #-}

-- V[y] - V[x] = w
ufpUnite :: (PrimMonad m, UM.Unbox p, U.Unbox v, Ix v, Eq p) => UnionFindPotential m v p -> v -> v -> p -> m (Maybe v)
ufpUnite ufp@UnionFindPotential{..} !x !y !w = do
  let !bnd = ufBnd ufpUF
      !ix = index bnd x
      !iy = index bnd y
  (!irx, !px) <- _ufpRoot ufp ix
  (!iry, !py) <- _ufpRoot ufp iy
  let !diffRyRx = ufpOp w $ ufpOp px $ ufpInv py

  if irx /= iry then do
    !r <- ufUnite ufpUF x y
    let !ir = index bnd r
    if ir == irx then 
      UM.unsafeWrite ufpDiff iry $! ufpInv diffRyRx
    else
      UM.unsafeWrite ufpDiff irx diffRyRx
    return $ Just r
  else do
    if diffRyRx == ufpUnit then do
      !r <- ufUnite ufpUF x y
      return $ Just r
    else return Nothing
{-# INLINE ufpUnite #-}

ufpUnite_ :: (PrimMonad m, UM.Unbox p, UM.Unbox v, Ix v, Eq p) => UnionFindPotential m v p -> v -> v -> p -> m ()
ufpUnite_ !ufp !x !y !w = void $ ufpUnite ufp x y w
{-# INLINE ufpUnite_ #-}

ufpIsSame :: (PrimMonad m, Ix v) => UnionFindPotential m v p -> v -> v -> m Bool
ufpIsSame UnionFindPotential{..} !x !y = ufIsSame ufpUF x y
{-# INLINE ufpIsSame #-}

ufpSize :: (PrimMonad m, Ix v) => UnionFindPotential m v p -> v -> m Int
ufpSize UnionFindPotential{..} !x = ufSize ufpUF x
{-# INLINE ufpSize #-}

ufpNumEdge :: (PrimMonad m, Ix v) => UnionFindPotential m v p -> v -> m Int
ufpNumEdge UnionFindPotential{..} !x = ufNumEdge ufpUF x
{-# INLINE ufpNumEdge #-}

ufpNumComponent :: PrimMonad m => UnionFindPotential m v p -> m Int
ufpNumComponent UnionFindPotential{..} = ufNumComponent ufpUF
{-# INLINE ufpNumComponent #-}