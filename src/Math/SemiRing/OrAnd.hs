{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.OrAnd where
import Data.Bits
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype OrAnd = OrAnd {unOrAnd :: Int} deriving (Eq, Show)
instance SemiRing OrAnd where
  rZero = OrAnd 0
  rOne = OrAnd (-1)
  (<+>) (OrAnd !x) (OrAnd !y) = OrAnd (x .|. y)
  (<.>) (OrAnd !x) (OrAnd !y) = OrAnd (x .&. y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

newtype instance UM.MVector s OrAnd = MV_OrAnd (UM.MVector s Int)
newtype instance U.Vector OrAnd = V_OrAnd (U.Vector Int)
instance U.Unbox OrAnd
instance GM.MVector UM.MVector OrAnd where
  basicLength (MV_OrAnd v) = GM.basicLength v
  basicUnsafeSlice i l (MV_OrAnd v) = MV_OrAnd (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_OrAnd v1) (MV_OrAnd v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_OrAnd <$> GM.basicUnsafeNew l
  basicInitialize (MV_OrAnd v) = GM.basicInitialize v
  basicUnsafeRead (MV_OrAnd v) i = OrAnd <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_OrAnd v) i (OrAnd x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector OrAnd where
  basicUnsafeFreeze (MV_OrAnd v) = V_OrAnd <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_OrAnd v) = MV_OrAnd <$> G.basicUnsafeThaw v
  basicLength (V_OrAnd v) = G.basicLength v
  basicUnsafeSlice i l (V_OrAnd v) = V_OrAnd (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_OrAnd v) i = OrAnd <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_OrAnd mv) (V_OrAnd v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}