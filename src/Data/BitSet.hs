{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BitSet where
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word

import Common.Template

data BitSet = BitSet 
  { bsNBits :: !Int,
    bsNWords :: !Int,
    bsWords :: !(U.Vector Word64)
  }
data MBitSet m = MBitSet
  { mbsNBits :: !Int,
    mbsNWords :: !Int,
    mbsWords :: !(UM.MVector (PrimState m) Word64)
  }

_bsNW :: Int -> Int
_bsNW !n = unsafeShiftR (n + 63) 6
{-# INLINE _bsNW #-}

bsZero :: Int -> BitSet
bsZero !n = 
  let !nw = _bsNW n
  in BitSet n nw $ U.replicate nw 0
{-# INLINE bsZero #-}

bsNull :: BitSet -> Bool
bsNull BitSet{..} = U.all (== 0) bsWords
{-# INLINE bsNull #-}

bsSet, bsClear, bsFlip :: BitSet -> Int -> BitSet
bsSet BitSet{..} !i = BitSet bsNBits bsNWords $ U.modify (\v -> UM.unsafeModify v (.|. bit (i .&. 63)) (unsafeShiftR i 6)) bsWords
bsClear BitSet{..} !i = BitSet bsNBits bsNWords $ U.modify (\v -> UM.unsafeModify v (.&. complement (bit (i .&. 63))) (unsafeShiftR i 6)) bsWords
bsFlip BitSet{..} !i = BitSet bsNBits bsNWords $ U.modify (\v -> UM.unsafeModify v (`xor` bit (i .&. 63)) (unsafeShiftR i 6)) bsWords
{-# INLINE bsSet #-}
{-# INLINE bsClear #-}
{-# INLINE bsFlip #-}

bsTestBit :: BitSet -> Int -> Bool
bsTestBit BitSet{..} !i = testBit (U.unsafeIndex bsWords (unsafeShiftR i 6)) (i .&. 63)
{-# INLINE bsTestBit #-}

_bsBinOp :: (Word64 -> Word64 -> Word64) -> BitSet -> BitSet -> BitSet
_bsBinOp !op (BitSet !n !nw !v1) (BitSet _ _ !v2) = BitSet n nw (U.zipWith op v1 v2)
{-# INLINE _bsBinOp #-}

bsXOR, bsAND, bsOR :: BitSet -> BitSet -> BitSet
bsXOR = _bsBinOp xor
bsAND = _bsBinOp (.&.)
bsOR  = _bsBinOp (.|.)
{-# INLINE bsXOR #-}
{-# INLINE bsAND #-}
{-# INLINE bsOR #-}

bsPopCount :: BitSet -> Int
bsPopCount BitSet{..} = U.foldl' (\ !acc !w -> acc + popCount w) 0 bsWords
{-# INLINE bsPopCount #-}

_bsMask :: BitSet -> BitSet
_bsMask bs@BitSet{..} = 
  let !remBits = bsNBits .&. 63
  in
    if remBits == 0 then bs
    else 
      let !m = bit remBits - 1
          !newWords = U.modify (\v -> UM.unsafeModify v (.&. m) (bsNWords - 1)) bsWords
      in BitSet bsNBits bsNWords newWords
{-# INLINE _bsMask #-}

bsShiftL :: BitSet -> Int -> BitSet
bsShiftL bs@BitSet{..} !s
  | s < 0 = error "bsShiftL : negative shift"
  | s == 0 = bs
  | s >= bsNBits = bsZero bsNBits
  | otherwise = 
      let (!q, !r) = divMod s 64
          !rInv = 64 - r
          !newWords = U.generate bsNWords $ \ !i ->
            if i - q >= 0 then
              let !v = U.unsafeIndex bsWords (i - q)
                  !low = unsafeShiftL v r
                  !high = 
                    if i - q - 1 >= 0 && r > 0 then unsafeShiftR (U.unsafeIndex bsWords (i - q - 1)) rInv
                    else 0
              in low .|. high
            else 0
          !res = BitSet bsNBits bsNWords newWords
      in _bsMask res
{-# INLINE bsShiftL #-}

bsShiftR :: BitSet -> Int -> BitSet
bsShiftR bs@BitSet{..} !s
  | s < 0 = error "bsShiftR : negative shift"
  | s == 0 = bs
  | s >= bsNBits = bsZero bsNBits
  | otherwise = 
      let (!q, !r) = divMod s 64
          !rInv = 64 - r
          !newWords = U.generate bsNWords $ \ !i -> 
            if i + q < bsNWords then
              let !v = U.unsafeIndex bsWords (i + q)
                  !high = unsafeShiftR v r
                  !low = 
                    if i + q + 1 < bsNWords && r > 0 then unsafeShiftL (U.unsafeIndex bsWords (i + q + 1)) rInv
                    else 0
              in high .|. low
            else 0
      in BitSet bsNBits bsNWords newWords
{-# INLINE bsShiftR #-}

mbsZero :: PrimMonad m => Int -> m (MBitSet m)
mbsZero !n = do
  let !nw = _bsNW n
  MBitSet n nw <$> UM.replicate nw 0
{-# INLINE mbsZero #-}

mbsNull :: PrimMonad m => MBitSet m -> m Bool
mbsNull MBitSet{..} = go 0
  where
    go !i
      | i == mbsNWords = return True
      | otherwise = do
          !w <- UM.unsafeRead mbsWords i
          if w == 0 then go (i + 1)
          else return False
{-# INLINE mbsNull #-}
{-# SPECIALIZE mbsNull :: MBitSet (ST s) -> ST s Bool #-}
{-# SPECIALIZE mbsNull :: MBitSet IO -> IO Bool #-}

mbsSet, mbsClear, mbsFlip :: PrimMonad m => MBitSet m -> Int -> m ()
mbsSet mbs@MBitSet{..} !i = do
  UM.unsafeModify mbsWords (.|. bit (i .&. 63)) (unsafeShiftR i 6)
  _mbsMask mbs
mbsClear MBitSet{..} !i = UM.unsafeModify mbsWords (.&. complement (bit (i .&. 63))) (unsafeShiftR i 6)
mbsFlip MBitSet{..} !i = UM.unsafeModify mbsWords (xor (bit (i .&. 63))) (unsafeShiftR i 6)
{-# INLINE mbsSet #-}
{-# INLINE mbsClear #-}
{-# INLINE mbsFlip #-}

mbsTestBit :: PrimMonad m => MBitSet m -> Int -> m Bool
mbsTestBit MBitSet{..} !i = do
  !w <- UM.unsafeRead mbsWords (unsafeShiftR i 6)
  return $ testBit w (i .&. 63)
{-# INLINE mbsTestBit #-}

-- 1つ目に2つ目をマージする
_mbsBinOp :: PrimMonad m => (Word64 -> Word64 -> Word64) -> MBitSet m -> MBitSet m -> m ()
_mbsBinOp op (MBitSet _ !nw !d) (MBitSet _ _ !s) = do
  forLoop 0 (== nw) succ $ \ !i -> do
    !si <- UM.unsafeRead s i
    UM.unsafeModify d (`op` si) i
{-# INLINE _mbsBinOp #-}
{-# SPECIALIZE _mbsBinOp :: (Word64 -> Word64 -> Word64) -> MBitSet (ST s) -> MBitSet (ST s) -> ST s () #-}
{-# SPECIALIZE _mbsBinOp :: (Word64 -> Word64 -> Word64) -> MBitSet IO -> MBitSet IO -> IO () #-}

mbsXOR, mbsAND, mbsOR :: PrimMonad m => MBitSet m -> MBitSet m -> m ()
mbsXOR = _mbsBinOp xor
mbsAND = _mbsBinOp (.&.)
mbsOR = _mbsBinOp (.|.)
{-# INLINE mbsXOR #-}
{-# INLINE mbsAND #-}
{-# INLINE mbsOR #-}

mbsCopy, mbsUnsafeCopy :: PrimMonad m => MBitSet m -> MBitSet m -> m ()
mbsCopy (MBitSet _ _ !d) (MBitSet _ _ !s) = UM.copy d s
mbsUnsafeCopy (MBitSet _ _ !d) (MBitSet _ _ !s) = UM.unsafeCopy d s
{-# INLINE mbsCopy #-}
{-# INLINE mbsUnsafeCopy #-}

-- 0リセット
mbsReset :: PrimMonad m => MBitSet m -> m ()
mbsReset MBitSet{..} = UM.set mbsWords 0
{-# INLINE mbsReset #-}

mbsPopCount :: PrimMonad m => MBitSet m -> m Int
mbsPopCount MBitSet{..} = do
  forLoopFold 0 (== mbsNWords) succ 0 $ \ !acc !i -> do
    !w <- UM.unsafeRead mbsWords i
    return $! acc + popCount w
{-# INLINE mbsPopCount #-}
{-# SPECIALIZE mbsPopCount :: MBitSet (ST s) -> ST s Int #-}
{-# SPECIALIZE mbsPopCount :: MBitSet IO -> IO Int #-}

_mbsMask :: PrimMonad m => MBitSet m -> m ()
_mbsMask MBitSet{..} = do
  let !remBits = mbsNBits .&. 63
  unless (remBits == 0) do
    let !m = bit remBits - 1
    UM.unsafeModify mbsWords (.&. m) (mbsNWords - 1)
{-# INLINE _mbsMask #-}

mbsShiftL :: PrimMonad m => MBitSet m -> Int -> m ()
mbsShiftL mbs@MBitSet{..} !s
  | s < 0 = error "mbsShiftL : negative shift"
  | s == 0 = return ()
  | s >= mbsNBits = mbsReset mbs
  | otherwise = do
      let (!q, !r) = divMod s 64
          !rInv = 64 - r
      
      forLoop (mbsNWords - 1) (< 0) pred $ \ !i -> do
        !val <- 
          if i - q >= 0 then do
            !v <- UM.unsafeRead mbsWords (i - q)
            let !low = unsafeShiftL v r
            !high <-
              if i - q - 1 >= 0 && r > 0 then (`unsafeShiftR` rInv) <$> UM.unsafeRead mbsWords (i - q - 1)
              else return 0
            return $ low .|. high
          else return 0
        UM.unsafeWrite mbsWords i val
      
      _mbsMask mbs
{-# INLINE mbsShiftL #-}
{-# SPECIALIZE mbsShiftL :: MBitSet (ST s) -> Int -> ST s () #-}
{-# SPECIALIZE mbsShiftL :: MBitSet IO -> Int -> IO () #-}

mbsShiftR :: PrimMonad m => MBitSet m -> Int -> m ()
mbsShiftR mbs@MBitSet{..} !s
  | s < 0 = error "mbsShiftR : negative shift"
  | s == 0 = return ()
  | s >= mbsNBits = mbsReset mbs
  | otherwise = do
      let (!q, !r) = divMod s 64
          !rInv = 64 - r
      
      forLoop 0 (== mbsNWords) succ $ \ !i -> do
        !val <-
          if i + q < mbsNWords then do
            !v <- UM.unsafeRead mbsWords (i + q)
            let !high = unsafeShiftR v r
            !low <-
              if i + q + 1 < mbsNWords && r > 0 then (`unsafeShiftL` rInv) <$> UM.unsafeRead mbsWords (i + q + 1)
              else return 0
            return $ high .|. low
          else return 0
        UM.unsafeWrite mbsWords i val
{-# INLINE mbsShiftR #-}
{-# SPECIALIZE mbsShiftR :: MBitSet (ST s) -> Int -> ST s () #-}
{-# SPECIALIZE mbsShiftR :: MBitSet IO -> Int -> IO () #-}

bsFreeze, bsUnsafeFreeze :: PrimMonad m => MBitSet m -> m BitSet
bsFreeze MBitSet{..} = BitSet mbsNBits mbsNWords <$> U.freeze mbsWords
bsUnsafeFreeze MBitSet{..} = BitSet mbsNBits mbsNWords <$> U.unsafeFreeze mbsWords
{-# INLINE bsFreeze #-}
{-# INLINE bsUnsafeFreeze #-}

bsThaw, bsUnsafeThaw :: PrimMonad m => BitSet -> m (MBitSet m)
bsThaw BitSet{..} = MBitSet bsNBits bsNWords <$> U.thaw bsWords
bsUnsafeThaw BitSet{..} = MBitSet bsNBits bsNWords <$> U.unsafeThaw bsWords
{-# INLINE bsThaw #-}
{-# INLINE bsUnsafeThaw #-}