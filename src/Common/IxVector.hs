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

newMIV :: (PrimMonad m, UM.Unbox a, Ix i) => (i, i) -> a -> m (MIxVect m i a)
newMIV !bnds !val = do
  !vect <- UM.replicate (rangeSize bnds) val
  return $ MIxVect bnds vect
{-# INLINE newMIV #-}
{-# SPECIALIZE newMIV :: (UM.Unbox a, Ix i) => (i, i) -> a -> ST s (MIxVect (ST s) i a) #-}
{-# SPECIALIZE newMIV :: (UM.Unbox a, Ix i) => (i, i) -> a -> IO (MIxVect IO i a) #-}

newMIV_ :: (PrimMonad m, UM.Unbox a, Ix i) => (i, i) -> m (MIxVect m i a)
newMIV_ !bnds = do
  !vect <- UM.new (rangeSize bnds)
  return $ MIxVect bnds vect
{-# INLINE newMIV_ #-}
{-# SPECIALIZE newMIV_ :: (UM.Unbox a, Ix i) => (i, i) -> ST s (MIxVect (ST s) i a) #-}
{-# SPECIALIZE newMIV_ :: (UM.Unbox a, Ix i) => (i, i) -> IO (MIxVect IO i a) #-}

fromListIV :: U.Unbox a => (i, i) -> [a] -> IxVect i a
fromListIV !bnds !xs = IxVect bnds $ U.fromList xs
{-# INLINE fromListIV #-}

fromVectIV :: (i, i) -> U.Vector a -> IxVect i a
fromVectIV !bnds !v = IxVect bnds v
{-# INLINE fromVectIV #-}

setMIV :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> a -> m ()
setMIV MIxVect{..} !val = UM.set vectMIV val
{-# INLINE setMIV #-}

readMIV :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> i -> m a
readMIV MIxVect{..} !idx = UM.unsafeRead vectMIV (index bndsMIV idx)
{-# INLINE readMIV #-}
{-# SPECIALIZE readMIV :: (UM.Unbox a, Ix i) => MIxVect (ST s) i a -> i -> ST s a #-}
{-# SPECIALIZE readMIV :: (UM.Unbox a, Ix i) => MIxVect IO i a -> i -> IO a #-}

writeMIV :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> i -> a -> m ()
writeMIV MIxVect{..} !idx !val = UM.unsafeWrite vectMIV (index bndsMIV idx) val
{-# INLINE writeMIV #-}
{-# SPECIALIZE writeMIV :: (UM.Unbox a, Ix i) => MIxVect (ST s) i a -> i -> a -> ST s () #-}
{-# SPECIALIZE writeMIV :: (UM.Unbox a, Ix i) => MIxVect IO i a -> i -> a -> IO () #-}

modifyMIV :: (PrimMonad m, UM.Unbox a, Ix i) => MIxVect m i a -> (a -> a) -> i -> m ()
modifyMIV MIxVect{..} !f !idx = UM.unsafeModify vectMIV f (index bndsMIV idx)
{-# INLINE modifyMIV #-}
{-# SPECIALIZE modifyMIV :: (UM.Unbox a, Ix i) => MIxVect (ST s) i a -> (a -> a) -> i -> ST s () #-}
{-# SPECIALIZE modifyMIV :: (UM.Unbox a, Ix i) => MIxVect IO i a -> (a -> a) -> i -> IO () #-}

unsafeCopyMIV :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> MIxVect m i a -> m ()
unsafeCopyMIV !v !w = UM.unsafeCopy (vectMIV v) (vectMIV w)
{-# INLINE unsafeCopyMIV #-}
{-# SPECIALIZE unsafeCopyMIV :: UM.Unbox a => MIxVect (ST s) i a -> MIxVect (ST s) i a -> ST s () #-}
{-# SPECIALIZE unsafeCopyMIV :: UM.Unbox a => MIxVect IO i a -> MIxVect IO i a -> IO () #-}

unsafeCopyIV :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> IxVect i a -> m ()
unsafeCopyIV !v !w = U.unsafeCopy (vectMIV v) (vectIV w)
{-# INLINE unsafeCopyIV #-}
{-# SPECIALIZE unsafeCopyIV :: UM.Unbox a => MIxVect (ST s) i a -> IxVect i a -> ST s () #-}
{-# SPECIALIZE unsafeCopyIV :: UM.Unbox a => MIxVect IO i a -> IxVect i a -> IO () #-}

indexIV :: (U.Unbox a, Ix i) => IxVect i a -> i -> a
indexIV IxVect{..} !idx = U.unsafeIndex vectIV (index bndsIV idx)
{-# INLINE indexIV #-}

freezeIV :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> m (IxVect i a)
freezeIV MIxVect{..} = IxVect bndsMIV <$> U.freeze vectMIV
{-# INLINE freezeIV #-}
{-# SPECIALIZE freezeIV :: (UM.Unbox a) => MIxVect (ST s) i a -> ST s (IxVect i a) #-}
{-# SPECIALIZE freezeIV :: (UM.Unbox a) => MIxVect IO i a -> IO (IxVect i a) #-}

unsafeFreezeIV :: (PrimMonad m, UM.Unbox a) => MIxVect m i a -> m (IxVect i a)
unsafeFreezeIV MIxVect{..} = IxVect bndsMIV <$> U.unsafeFreeze vectMIV
{-# INLINE unsafeFreezeIV #-}
{-# SPECIALIZE unsafeFreezeIV :: (UM.Unbox a) => MIxVect (ST s) i a -> ST s (IxVect i a) #-}
{-# SPECIALIZE unsafeFreezeIV :: (UM.Unbox a) => MIxVect IO i a -> IO (IxVect i a) #-}

thawIV :: (PrimMonad m, UM.Unbox a) => IxVect i a -> m (MIxVect m i a)
thawIV IxVect{..} = MIxVect bndsIV <$> U.thaw vectIV
{-# INLINE thawIV #-}
{-# SPECIALIZE thawIV :: (UM.Unbox a) => IxVect i a -> ST s (MIxVect (ST s) i a) #-}
{-# SPECIALIZE thawIV :: (UM.Unbox a) => IxVect i a -> IO (MIxVect IO i a) #-}

unsafeThawIV :: (PrimMonad m, UM.Unbox a) => IxVect i a -> m (MIxVect m i a)
unsafeThawIV IxVect{..} = MIxVect bndsIV <$> U.unsafeThaw vectIV
{-# INLINE unsafeThawIV #-}
{-# SPECIALIZE unsafeThawIV :: (UM.Unbox a) => IxVect i a -> ST s (MIxVect (ST s) i a) #-}
{-# SPECIALIZE unsafeThawIV :: (UM.Unbox a) => IxVect i a -> IO (MIxVect IO i a) #-}

mapIV :: (U.Unbox a, U.Unbox b) => (a -> b) -> IxVect i a -> IxVect i b
mapIV !f IxVect{..} = IxVect bndsIV (U.map f vectIV)
{-# INLINE mapIV #-}

ixmapIV :: (Ix i, Ix j, U.Unbox a) => (i, i) -> (i -> j) -> IxVect j a -> IxVect i a
ixmapIV !bnds' !f !v = runST $ do
  !mv <- newMIV_ bnds'
  forM_ (range bnds') $ \ !i -> do
    writeMIV mv i $! indexIV v (f i)
  unsafeFreezeIV mv
{-# INLINE ixmapIV #-}

imapIV :: (Ix i, U.Unbox a, U.Unbox b) => (i -> a -> b) -> IxVect i a -> IxVect i b
imapIV !f v@IxVect{..} = runST $ do
  !mv <- newMIV_ bndsIV
  forM_ (range bndsIV) $ \ !i -> do
    writeMIV mv i $! f i (indexIV v i)
  unsafeFreezeIV mv
{-# INLINE imapIV #-}