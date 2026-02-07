module String.Transform where
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Char
import Data.Vector.Unboxed qualified as U

byteStringToIntVec :: Char -> Int -> BS.ByteString -> U.Vector Int
byteStringToIntVec !c0 !offset !bs = U.generate (BS.length bs) $ \ !i -> fromIntegral (BSU.unsafeIndex bs i) - ord c0 + offset
{-# INLINE byteStringToIntVec #-}

byteStringToRawIntVec :: BS.ByteString -> U.Vector Int
byteStringToRawIntVec !bs = U.generate (BS.length bs) $ \ !i -> fromIntegral (BSU.unsafeIndex bs i)
{-# INLINE byteStringToRawIntVec #-}