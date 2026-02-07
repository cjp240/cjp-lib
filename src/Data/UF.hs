{-# LANGUAGE RecordWildCards #-}
module Data.UF where
import Control.Monad
import Control.Monad.Primitive
import Data.Ix
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data UnionFind m v = UnionFind
  { ufBnd :: !(v, v),
    ufVs :: !(U.Vector v),
    ufParent :: UM.MVector (PrimState m) Int,
    ufNV :: UM.MVector (PrimState m) Int,
    ufNE :: UM.MVector (PrimState m) Int,
    ufNC :: MutVar (PrimState m) Int
  }

ufNew :: (PrimMonad m, Ix v, UM.Unbox v) => (v, v) -> m (UnionFind m v)
ufNew !bnd = do
  let !n = rangeSize bnd
      !vs = U.fromListN n $ range bnd
  parent <- UM.replicate n (-1)
  nV <- UM.replicate n 1
  nE <- UM.replicate n 0
  nC <- newMutVar n
  return $ UnionFind bnd vs parent nV nE nC

ufRoot :: (PrimMonad m, Ix v, U.Unbox v) => UnionFind m v -> v -> m v
ufRoot UnionFind{..} !x = do
  let !ix = index ufBnd x
  !ir <- _ufRoot ufParent ix
  return $! ufVs U.! ir
{-# INLINE ufRoot #-}

_ufRoot :: PrimMonad m => UM.MVector (PrimState m) Int -> Int -> m Int
_ufRoot !parent !ix = do
  !p <- UM.unsafeRead parent ix
  if p == -1 then return ix
  else do
    !root <- _ufRoot parent p
    UM.unsafeWrite parent ix root
    return root
{-# INLINE _ufRoot #-}

ufUnite :: (PrimMonad m, Ix v, UM.Unbox v) => UnionFind m v -> v -> v -> m v
ufUnite UnionFind{..} !x !y = do
  let !ix = index ufBnd x
      !iy = index ufBnd y
  !irx <- _ufRoot ufParent ix
  !iry <- _ufRoot ufParent iy
  !ex <- UM.unsafeRead ufNE irx
  !ey <- UM.unsafeRead ufNE iry

  if irx /= iry then do
    !vx <- UM.unsafeRead ufNV irx
    !vy <- UM.unsafeRead ufNV iry
    modifyMutVar' ufNC pred
    if vx > vy then do
      UM.unsafeWrite ufParent iry irx
      UM.unsafeWrite ufNV irx $! vx + vy
      UM.unsafeWrite ufNE irx $! ex + ey + 1
      return $! ufVs U.! irx
    else do
      UM.unsafeWrite ufParent irx iry
      UM.unsafeWrite ufNV iry $! vx + vy
      UM.unsafeWrite ufNE iry $! ex + ey + 1
      return $! ufVs U.! iry
  else do
    UM.unsafeModify ufNE succ irx
    return $! ufVs U.! irx
{-# INLINE ufUnite #-}

ufUnite_ :: (PrimMonad m, Ix v, UM.Unbox v) => UnionFind m v -> v -> v -> m ()
ufUnite_ !uf !x !y = void $ ufUnite uf x y
{-# INLINE ufUnite_ #-}

ufIsSame :: (PrimMonad m, Ix v) => UnionFind m v -> v -> v -> m Bool
ufIsSame UnionFind{..} !x !y = 
  (==) <$> _ufRoot ufParent (index ufBnd x) <*> _ufRoot ufParent (index ufBnd y)
{-# INLINE ufIsSame #-}

ufSize :: (PrimMonad m, Ix v) => UnionFind m v -> v -> m Int
ufSize UnionFind{..} !x = do
  let !ix = index ufBnd x
  !irx <- _ufRoot ufParent ix
  UM.unsafeRead ufNV irx
{-# INLINE ufSize #-}

ufNumEdge :: (PrimMonad m, Ix v) => UnionFind m v -> v -> m Int
ufNumEdge UnionFind{..} !x = do
  let !ix = index ufBnd x
  !irx <- _ufRoot ufParent ix
  UM.unsafeRead ufNE irx
{-# INLINE ufNumEdge #-}

ufNumComponent :: (PrimMonad m) => UnionFind m v -> m Int
ufNumComponent UnionFind{..} = readMutVar ufNC
{-# INLINE ufNumComponent #-}