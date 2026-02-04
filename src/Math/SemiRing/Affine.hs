{-# LANGUAGE TypeFamilies #-}
module Math.SemiRing.Affine where
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Math.SemiRing

-- f <.> g = f . g
data Affine a = Affine {affA :: !a, addB :: !a} deriving (Eq, Show)
instance SemiRing a => SemiRing (Affine a) where
  rZero = Affine rZero rZero
  rOne = Affine rOne rZero
  (<+>) (Affine !a1 !b1) (Affine !a2 !b2) = Affine (a1 <+> a2) (b1 <+> b2)
  (<.>) (Affine !a1 !b1) (Affine !a2 !b2) = Affine (a1 <.> a2) ((a1 <.> b2) <+> b1)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}

instance Ring a => Ring (Affine a) where
  rNegate (Affine !a !b) = Affine (rNegate a) (rNegate b)
  {-# INLINE rNegate #-}

newtype instance UM.MVector s (Affine a) = MV_Affine (UM.MVector s (a, a))
newtype instance U.Vector (Affine a) = V_Affine (U.Vector (a, a))
instance U.Unbox a => U.Unbox (Affine a)
instance U.Unbox a => GM.MVector UM.MVector (Affine a) where
  basicLength (MV_Affine v) = GM.basicLength v
  basicUnsafeSlice i l (MV_Affine v) = MV_Affine (GM.basicUnsafeSlice i l v)
  basicOverlaps (MV_Affine v1) (MV_Affine v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew l = MV_Affine <$> GM.basicUnsafeNew l
  basicInitialize (MV_Affine v) = GM.basicInitialize v
  basicUnsafeRead (MV_Affine v) i = do
    (a, b) <- GM.basicUnsafeRead v i
    return $ Affine a b
  basicUnsafeWrite (MV_Affine v) i (Affine a b) = GM.basicUnsafeWrite v i (a, b)
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

instance U.Unbox a => G.Vector U.Vector (Affine a) where
  basicUnsafeFreeze (MV_Affine v) = V_Affine <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Affine v) = MV_Affine <$> G.basicUnsafeThaw v
  basicLength (V_Affine v) = G.basicLength v
  basicUnsafeSlice i l (V_Affine v) = V_Affine (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Affine v) i = do
    (a, b) <- G.basicUnsafeIndexM v i
    return $ Affine a b
  basicUnsafeCopy (MV_Affine mv) (V_Affine v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}