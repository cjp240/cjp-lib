{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.AndOr where
import Data.Bits
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype AndOr = AndOr {unAndOr :: Int} deriving (Eq, Show)
instance SemiRing AndOr where
  rZero = AndOr (-1)
  rOne = AndOr 0
  (<+>) (AndOr !x) (AndOr !y) = AndOr (x .&. y)
  (<.>) (AndOr !x) (AndOr !y) = AndOr (x .|. y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s AndOr = MV_AndOr (UM.MVector s Int)
newtype instance U.Vector AndOr = V_AndOr (U.Vector Int)
instance U.Unbox AndOr
instance GM.MVector UM.MVector AndOr where
  basicLength (MV_AndOr v) = GM.basicLength v
  basicUnsafeSlice i l (MV_AndOr v) = MV_AndOr (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_AndOr v1) (MV_AndOr v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_AndOr <$> GM.basicUnsafeNew l
  basicInitialize (MV_AndOr v) = GM.basicInitialize v
  basicUnsafeRead (MV_AndOr v) i = AndOr <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_AndOr v) i (AndOr x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector AndOr where
  basicUnsafeFreeze (MV_AndOr v) = V_AndOr <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_AndOr v) = MV_AndOr <$> G.basicUnsafeThaw v
  basicLength (V_AndOr v) = G.basicLength v
  basicUnsafeSlice i l (V_AndOr v) = V_AndOr (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_AndOr v) i = AndOr <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_AndOr mv) (V_AndOr v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}