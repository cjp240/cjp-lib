{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.MaxPlus where
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype MaxPlus = MaxPlus {unMaxPlus :: Int} deriving (Eq, Show)
instance SemiRing MaxPlus where
  rZero = MaxPlus minBound
  rOne = MaxPlus 0
  (<+>) (MaxPlus !x) (MaxPlus !y) = MaxPlus (max x y)
  (<.>) (MaxPlus !x) (MaxPlus !y)
    | x == minBound || y == minBound = MaxPlus minBound
    | otherwise = MaxPlus (x + y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s MaxPlus = MV_MaxPlus (UM.MVector s Int)
newtype instance U.Vector MaxPlus = V_MaxPlus (U.Vector Int)
instance U.Unbox MaxPlus
instance GM.MVector UM.MVector MaxPlus where
  basicLength (MV_MaxPlus v) = GM.basicLength v
  basicUnsafeSlice i l (MV_MaxPlus v) = MV_MaxPlus (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_MaxPlus v1) (MV_MaxPlus v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_MaxPlus <$> GM.basicUnsafeNew l
  basicInitialize (MV_MaxPlus v) = GM.basicInitialize v
  basicUnsafeRead (MV_MaxPlus v) i = MaxPlus <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_MaxPlus v) i (MaxPlus x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector MaxPlus where
  basicUnsafeFreeze (MV_MaxPlus v) = V_MaxPlus <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_MaxPlus v) = MV_MaxPlus <$> G.basicUnsafeThaw v
  basicLength (V_MaxPlus v) = G.basicLength v
  basicUnsafeSlice i l (V_MaxPlus v) = V_MaxPlus (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_MaxPlus v) i = MaxPlus <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_MaxPlus mv) (V_MaxPlus v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
