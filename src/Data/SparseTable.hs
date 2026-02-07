{-# LANGUAGE RecordWildCards #-}
module Data.SparseTable where
import Control.Monad.ST
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template

-- op x x = x
data SparseTable a = SparseTable
  { sptTable :: !(U.Vector a),
    sptSize :: !Int,
    sptOp :: !(a -> a -> a)
  }

sptBuild :: U.Unbox a => (a -> a -> a) -> U.Vector a -> SparseTable a
sptBuild !op !v = runST $ do
  let !n = U.length v
      !sptLog = if n <= 1 then 0 else finiteBitSize n - countLeadingZeros n - 1
  
  t <- UM.unsafeNew ((sptLog + 1) * n)

  forLoop 0 (== n) succ $ \ !i -> do
    UM.unsafeWrite t i $! v U.! i

  forLoop 1 (> sptLog) succ $ \ !j -> do
    let !prevRow = (j - 1) * n
        !currRow = j * n
        !offset = shiftL 1 (j - 1)
    
    forLoop 0 (== n) succ $ \ !i -> do
      !v1 <- UM.unsafeRead t (prevRow + i)
      if i + offset < n 
        then do
          !v2 <- UM.unsafeRead t (prevRow + i + offset)
          UM.unsafeWrite t (currRow + i) $! op v1 v2
        else UM.unsafeWrite t (currRow + i) v1

  !table <- U.unsafeFreeze t
  return $ SparseTable table n op
{-# INLINABLE sptBuild #-}

sptProd :: U.Unbox a => SparseTable a -> Int -> Int -> a
sptProd SparseTable{..} !l !r
  = let !len = r - l
        !j = finiteBitSize len - countLeadingZeros len - 1
        !offset = shiftL 1 j
        !row = j * sptSize
        !v1 = U.unsafeIndex sptTable (row + l)
        !v2 = U.unsafeIndex sptTable (row + r - offset)
        !res = sptOp v1 v2
    in res
{-# INLINE sptProd #-}