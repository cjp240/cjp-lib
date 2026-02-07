{-# LANGUAGE RecordWildCards #-}
module Common.IxVector where
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Ix
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data MIxVect m i a = MIxVect
  { bndsMIV :: !(i, i),
    vectMIV :: !(UM.MVector (PrimState m) a)
  }

data IxVect i a = IxVect
  { bndsIV :: !(i, i),
    vectIV :: !(U.Vector a)
  }

mivNew :: (PrimMonad m, UM.Unbox a, Ix i) => (i, i) -> a -> m (MIxVect m i a)
mivNew !bnds !val = do
  !vect <- UM.replicate (rangeSize bnds) val
  return $ MIxVect bnds vect
{-# INLINE mivNew #-}

mivNew_ :: (PrimMonad m, UM.Unbox a, Ix i) => (i, i) -> m (MIxVect m i a)
mivNew_ !bnds = do
  !vect <- UM.new (rangeSize bnds)
  return $ MIxVect bnds vect
{-# INLINE mivNew_ #-}

ivFromList :: U.Unbox a => (i, i) -> [a] -> IxVect i a
ivFromList !bnds !xs = IxVect bnds $ U.fromList xs
{-# INLINE ivFromList #-}

ivFromVect :: (i, i) -> U.Vector a -> IxVect i a
ivFromVect !bnds !v = IxVect bnds v
{-# INLINE ivFromVect #-}

mivSet :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> a -> m ()
mivSet MIxVect{..} !val = UM.set vectMIV val
{-# INLINE mivSet #-}

mivRead :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> i -> m a
mivRead MIxVect{..} !idx = UM.unsafeRead vectMIV (index bndsMIV idx)
{-# INLINE mivRead #-}

mivWrite :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> i -> a -> m ()
mivWrite MIxVect{..} !idx !val = UM.unsafeWrite vectMIV (index bndsMIV idx) val
{-# INLINE mivWrite #-}

mivModify :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> (a -> a) -> i -> m ()
mivModify MIxVect{..} !f !idx = UM.unsafeModify vectMIV f (index bndsMIV idx)
{-# INLINE mivModify #-}

mivUnsafeCopy :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> MIxVect m i a -> m ()
mivUnsafeCopy !v !w = UM.unsafeCopy (vectMIV v) (vectMIV w)
{-# INLINE mivUnsafeCopy #-}

ivUnsafeCopy :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> IxVect i a -> m ()
ivUnsafeCopy !v !w = U.unsafeCopy (vectMIV v) (vectIV w)
{-# INLINE ivUnsafeCopy #-}

ivIndex :: (U.Unbox a, Ix i) => IxVect i a -> i -> a
ivIndex IxVect{..} !idx = U.unsafeIndex vectIV (index bndsIV idx)
{-# INLINE ivIndex #-}

ivFreeze :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> m (IxVect i a)
ivFreeze MIxVect{..} = IxVect bndsMIV <$> U.freeze vectMIV
{-# INLINE ivFreeze #-}

ivUnsafeFreeze :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> m (IxVect i a)
ivUnsafeFreeze MIxVect{..} = IxVect bndsMIV <$> U.unsafeFreeze vectMIV
{-# INLINE ivUnsafeFreeze #-}

ivThaw :: (PrimMonad m, UM.Unbox a) => IxVect i a -> m (MIxVect m i a)
ivThaw IxVect{..} = MIxVect bndsIV <$> U.thaw vectIV
{-# INLINE ivThaw #-}

ivUnsafeThaw :: (PrimMonad m, UM.Unbox a) => IxVect i a -> m (MIxVect m i a)
ivUnsafeThaw IxVect{..} = MIxVect bndsIV <$> U.unsafeThaw vectIV
{-# INLINE ivUnsafeThaw #-}

ivMap :: (U.Unbox a, U.Unbox b) => (a -> b) -> IxVect i a -> IxVect i b
ivMap !f IxVect{..} = IxVect bndsIV (U.map f vectIV)
{-# INLINE ivMap #-}

ivIxMap :: (Ix i, Ix j, U.Unbox a) => (i, i) -> (i -> j) -> IxVect j a -> IxVect i a
ivIxMap !bnds' !f !v = runST $ do
  !mv <- mivNew_ bnds'
  forM_ (range bnds') $ \ !i -> do
    mivWrite mv i $! ivIndex v (f i)
  ivUnsafeFreeze mv
{-# INLINE ivIxMap #-}

ivIMap :: (Ix i, U.Unbox a, U.Unbox b) => (i -> a -> b) -> IxVect i a -> IxVect i b
ivIMap !f v@IxVect{..} = runST $ do
  !mv <- mivNew_ bndsIV
  forM_ (range bndsIV) $ \ !i -> do
    mivWrite mv i $! f i (ivIndex v i)
  ivUnsafeFreeze mv
{-# INLINE ivIMap #-}