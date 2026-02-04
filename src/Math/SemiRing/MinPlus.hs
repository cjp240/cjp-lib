{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.MinPlus where
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype MinPlus = MinPlus {unMinPlus :: Int} deriving (Eq, Show)
instance SemiRing MinPlus where
  rZero = MinPlus maxBound
  rOne = MinPlus 0
  (<+>) (MinPlus !x) (MinPlus !y) = MinPlus (min x y)
  (<.>) (MinPlus !x) (MinPlus !y)
    | x == maxBound || y == maxBound = MinPlus maxBound
    | otherwise = MinPlus (x + y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s MinPlus = MV_MinPlus (UM.MVector s Int)
newtype instance U.Vector MinPlus = V_MinPlus (U.Vector Int)
instance U.Unbox MinPlus
instance GM.MVector UM.MVector MinPlus where
  basicLength (MV_MinPlus v) = GM.basicLength v
  basicUnsafeSlice i l (MV_MinPlus v) = MV_MinPlus (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_MinPlus v1) (MV_MinPlus v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_MinPlus <$> GM.basicUnsafeNew l
  basicInitialize (MV_MinPlus v) = GM.basicInitialize v
  basicUnsafeRead (MV_MinPlus v) i = MinPlus <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_MinPlus v) i (MinPlus x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector MinPlus where
  basicUnsafeFreeze (MV_MinPlus v) = V_MinPlus <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_MinPlus v) = MV_MinPlus <$> G.basicUnsafeThaw v
  basicLength (V_MinPlus v) = G.basicLength v
  basicUnsafeSlice i l (V_MinPlus v) = V_MinPlus (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_MinPlus v) i = MinPlus <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_MinPlus mv) (V_MinPlus v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}