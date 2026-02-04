{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.XorAnd where
import Data.Bits
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

newtype XorAnd = XorAnd {unXorAnd :: Int} deriving (Eq, Show)
instance SemiRing XorAnd where
  rZero = XorAnd 0
  rOne = XorAnd (-1)
  (<+>) (XorAnd !x) (XorAnd !y) = XorAnd (xor x y)
  (<.>) (XorAnd !x) (XorAnd !y) = XorAnd (x .&. y)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
instance Ring XorAnd where
  rNegate = id
  {-# INLINE rNegate #-}

newtype instance UM.MVector s XorAnd = MV_XorAnd (UM.MVector s Int)
newtype instance U.Vector XorAnd = V_XorAnd (U.Vector Int)
instance U.Unbox XorAnd
instance GM.MVector UM.MVector XorAnd where
  basicLength (MV_XorAnd v) = GM.basicLength v
  basicUnsafeSlice i l (MV_XorAnd v) = MV_XorAnd (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_XorAnd v1) (MV_XorAnd v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_XorAnd <$> GM.basicUnsafeNew l
  basicInitialize (MV_XorAnd v) = GM.basicInitialize v
  basicUnsafeRead (MV_XorAnd v) i = XorAnd <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_XorAnd v) i (XorAnd x) = GM.basicUnsafeWrite v i x
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance G.Vector U.Vector XorAnd where
  basicUnsafeFreeze (MV_XorAnd v) = V_XorAnd <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_XorAnd v) = MV_XorAnd <$> G.basicUnsafeThaw v
  basicLength (V_XorAnd v) = G.basicLength v
  basicUnsafeSlice i l (V_XorAnd v) = V_XorAnd (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_XorAnd v) i = XorAnd <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_XorAnd mv) (V_XorAnd v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}