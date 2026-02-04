{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.MaxMin where
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype MaxMin = MaxMin {unMaxMin :: Int} deriving (Eq, Show)
instance SemiRing MaxMin where
  rZero = MaxMin minBound
  rOne = MaxMin maxBound
  (<+>) (MaxMin !x) (MaxMin !y) = MaxMin (max x y)
  (<.>) (MaxMin !x) (MaxMin !y) = MaxMin (min x y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s MaxMin = MV_MaxMin (UM.MVector s Int)
newtype instance U.Vector MaxMin = V_MaxMin (U.Vector Int)
instance U.Unbox MaxMin
instance GM.MVector UM.MVector MaxMin where
  basicLength (MV_MaxMin v) = GM.basicLength v
  basicUnsafeSlice i l (MV_MaxMin v) = MV_MaxMin (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_MaxMin v1) (MV_MaxMin v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_MaxMin <$> GM.basicUnsafeNew l
  basicInitialize (MV_MaxMin v) = GM.basicInitialize v
  basicUnsafeRead (MV_MaxMin v) i = MaxMin <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_MaxMin v) i (MaxMin x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector MaxMin where
  basicUnsafeFreeze (MV_MaxMin v) = V_MaxMin <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_MaxMin v) = MV_MaxMin <$> G.basicUnsafeThaw v
  basicLength (V_MaxMin v) = G.basicLength v
  basicUnsafeSlice i l (V_MaxMin v) = V_MaxMin (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_MaxMin v) i = MaxMin <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_MaxMin mv) (V_MaxMin v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}