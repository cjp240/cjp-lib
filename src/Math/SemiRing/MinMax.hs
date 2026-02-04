{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.MinMax where
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype MinMax = MinMax {unMinMax :: Int} deriving (Eq, Show)
instance SemiRing MinMax where
  rZero = MinMax maxBound
  rOne = MinMax minBound
  (<+>) (MinMax !x) (MinMax !y) = MinMax (min x y)
  (<.>) (MinMax !x) (MinMax !y) = MinMax (max x y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s MinMax = MV_MinMax (UM.MVector s Int)
newtype instance U.Vector MinMax = V_MinMax (U.Vector Int)
instance U.Unbox MinMax
instance GM.MVector UM.MVector MinMax where
  basicLength (MV_MinMax v) = GM.basicLength v
  basicUnsafeSlice i l (MV_MinMax v) = MV_MinMax (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_MinMax v1) (MV_MinMax v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_MinMax <$> GM.basicUnsafeNew l
  basicInitialize (MV_MinMax v) = GM.basicInitialize v
  basicUnsafeRead (MV_MinMax v) i = MinMax <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_MinMax v) i (MinMax x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector MinMax where
  basicUnsafeFreeze (MV_MinMax v) = V_MinMax <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_MinMax v) = MV_MinMax <$> G.basicUnsafeThaw v
  basicLength (V_MinMax v) = G.basicLength v
  basicUnsafeSlice i l (V_MinMax v) = V_MinMax (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_MinMax v) i = MinMax <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_MinMax mv) (V_MinMax v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}