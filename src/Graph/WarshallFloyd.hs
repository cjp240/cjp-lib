{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Graph.WarshallFloyd where
import Control.Monad
import Control.Monad.Primitive
import Data.Ix
import Data.Vector.Unboxed qualified as U

import Common.IxVector

data WFData m v = WFData
  { wfBnd :: !(v, v),
    wfVs :: !(U.Vector v),
    wfDist :: MIxVect m (v, v) Int
  }

wfBuild :: (PrimMonad m, U.Unbox v, Ix v) => (v, v) -> U.Vector (v, v, Int) -> m (WFData m v)
wfBuild !bnd !edges = do
  let !inf = div (maxBound :: Int) 2
      !bnd2 = ((fst bnd, fst bnd), (snd bnd, snd bnd))
      !n = rangeSize bnd
      !vs = U.fromListN n $ range bnd
  
  dist <- mivNew bnd2 inf
  
  U.forM_ vs $ \ !u -> mivWrite dist (u, u) 0
  U.forM_ edges $ \(!u, !v, !w) -> mivModify dist (min w) (u, v)

  U.forM_ vs $ \ !k -> do
    U.forM_ vs $ \ !i -> do
      !dik <- mivRead dist (i, k)
      when (dik < inf) do
        U.forM_ vs $ \ !j -> do
          !dkj <- mivRead dist (k, j)
          when (dkj < inf) do
            let !newD = dik + dkj
            mivModify dist (min newD) (i, j)

  return $ WFData bnd vs dist

wfBuildUn :: (PrimMonad m, U.Unbox v, Ix v) => (v, v) -> U.Vector (v, v, Int) -> m (WFData m v)
wfBuildUn !bnd !edges = do
  let !edges2 = edges U.++ U.map (\(!u, !v, !w) -> (v, u, w)) edges
  wfBuild bnd edges2

wfAddEdge :: (PrimMonad m, U.Unbox v, Ix v) => WFData m v -> (v, v, Int) -> m ()
wfAddEdge WFData{..} (!u, !v, !w) = do
  let !inf = div (maxBound :: Int) 2
  !duvOld <- mivRead wfDist (u, v)
  when (duvOld > w) do
    mivWrite wfDist (u, v) w

    U.forM_ wfVs $ \ !i -> do
      !diu <- mivRead wfDist (i, u)
      when (diu < inf) do
        U.forM_ wfVs $ \ !j -> do
          !dvj <- mivRead wfDist (v, j)
          when (dvj < inf) do
            let !newD = diu + w + dvj
            mivModify wfDist (min newD) (i, j)
{-# INLINABLE wfAddEdge #-}

wfAddEdgeUn :: (PrimMonad m, U.Unbox v, Ix v) => WFData m v -> (v, v, Int) -> m ()
wfAddEdgeUn WFData{..} (!u, !v, !w) = do
  let !inf = div (maxBound :: Int) 2
  !duvOld <- mivRead wfDist (u, v)
  !dvuOld <- mivRead wfDist (v, u)
  when (duvOld > w || dvuOld > w) do
    let !duv = min duvOld w
        !dvu = min dvuOld w
    mivWrite wfDist (u, v) duv
    mivWrite wfDist (v, u) dvu

    U.forM_ wfVs $ \ !i -> do
      !diu <- mivRead wfDist (i, u)
      !div' <- mivRead wfDist (i, v)
      when (diu < inf || div' < inf) do
        U.forM_ wfVs $ \ !j -> do
          !duj <- mivRead wfDist (u, j)
          !dvj <- mivRead wfDist (v, j)
          when (duj < inf || dvj < inf) do
            let !d1 = if diu < inf && dvj < inf then diu + duv + dvj else inf
                !d2 = if div' < inf && duj < inf then div' + dvu + duj else inf
            mivModify wfDist (min (min d1 d2)) (i, j)
{-# INLINABLE wfAddEdgeUn #-}

wfGetDist :: (PrimMonad m, Ix v) => WFData m v -> v -> v -> m Int
wfGetDist WFData{..} !u !v = mivRead wfDist (u, v)
{-# INLINE wfGetDist #-}

wfFreeze :: (PrimMonad m, Ix v) => WFData m v -> m (IxVect (v, v) Int)
wfFreeze WFData{..} = ivUnsafeFreeze wfDist