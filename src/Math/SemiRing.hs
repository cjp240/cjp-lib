module Math.SemiRing where

class SemiRing a where
  rZero :: a
  rOne :: a
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a

class SemiRing a => Ring a where
  rNegate :: a -> a

(<->) :: Ring a => a -> a -> a
x <-> y = x <+> rNegate y
{-# INLINE (<->) #-}

rSum :: (SemiRing a, Foldable t) => t a -> a
rSum = foldr (<+>) rZero
{-# INLINE rSum #-}

rProduct :: (SemiRing a, Foldable t) => t a -> a
rProduct = foldr (<.>) rOne
{-# INLINE rProduct #-}

instance SemiRing Int where
  rZero = 0
  rOne = 1
  (<+>) = (+)
  (<.>) = (*)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
instance Ring Int where
  rNegate = negate
  {-# INLINE rNegate #-}

instance SemiRing Double where
  rZero = 0
  rOne = 1
  (<+>) = (+)
  (<.>) = (*)
  {-# INLINE rZero #-}
  {-# INLINE rOne #-}
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
instance Ring Double where
  rNegate = negate
  {-# INLINE rNegate #-}