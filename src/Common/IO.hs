module Common.IO where
import Control.Monad
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Maybe
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import System.IO
import Common.IxVector
import Common.Template

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------
readintErr :: BS.ByteString -> (Int, BS.ByteString)
readintErr !s = case BS.readInt $ BS.dropWhile isSpace s of
  Just (!n, !r) -> (n, r)
  _ -> error "readintErr : failed"
{-# INLINE readintErr #-}
getint :: IO Int
getint = do
  !s <- BS.getLine
  let (!n, _) = readintErr s
  return n
{-# INLINE getint #-}
getintU :: IO (U.Vector Int)
getintU = U.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
{-# INLINE getintU #-}
getintTuple :: IO (Int, Int)
getintTuple = do
  !s <- BS.getLine
  let (!x, !r) = readintErr s
      (!y, _) = readintErr r
  return (x, y)
{-# INLINE getintTuple #-}
getintNTuplesU :: Int -> IO (U.Vector (Int, Int))
getintNTuplesU n = U.replicateM n getintTuple
{-# INLINE getintNTuplesU #-}
getintTuple3 :: IO (Int, Int, Int)
getintTuple3 = do
  !s <- BS.getLine
  let (!x, !r1) = readintErr s
      (!y, !r2) = readintErr r1
      (!z, _) = readintErr r2
  return (x, y, z)
{-# INLINE getintTuple3 #-}
getintNTuples3U :: Int -> IO (U.Vector (Int, Int, Int))
getintNTuples3U !n = U.replicateM n getintTuple3
{-# INLINE getintNTuples3U #-}
getintList :: IO [Int]
getintList = map fst . mapMaybe BS.readInt . BS.words <$> BS.getLine
{-# INLINE getintList #-}
getintNList :: Int -> IO [[Int]]
getintNList n = replicateM n getintList
{-# INLINE getintNList #-}
readIntErr :: BS.ByteString -> (Integer, BS.ByteString)
readIntErr !s = case BS.readInteger $ BS.dropWhile isSpace s of
  Just (!n, !r) -> (n, r)
  _ -> error "readIntErr : failed"
{-# INLINE readIntErr #-}
getInt :: IO Integer
getInt = do
  !s <- BS.getLine
  let (!n, _) = readIntErr s
  return n
{-# INLINE getInt #-}
getIntV :: IO (V.Vector Integer)
getIntV = V.unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
{-# INLINE getIntV #-}
getIntList :: IO [Integer]
getIntList = map fst . mapMaybe BS.readInteger . BS.words <$> BS.getLine
{-# INLINE getIntList #-}
getIntNList :: Int -> IO [[Integer]]
getIntNList n = replicateM n (map fst . mapMaybe BS.readInteger . BS.words <$> BS.getLine)
{-# INLINE getIntNList #-}
getIntTuple :: IO (Integer, Integer)
getIntTuple = do
  !s <- BS.getLine
  let (!x, !r) = readIntErr s
      (!y, _) = readIntErr r
  return (x, y)
{-# INLINE getIntTuple #-}
getIntNTuplesV :: Int -> IO (V.Vector (Integer, Integer))
getIntNTuplesV n = V.replicateM n getIntTuple
{-# INLINE getIntNTuplesV #-}
getIntTuple3 :: IO (Integer, Integer, Integer)
getIntTuple3 = do
  !s <- BS.getLine
  let (!x, !r1) = readIntErr s
      (!y, !r2) = readIntErr r1
      (!z, _) = readIntErr r2
  return (x, y, z)
{-# INLINE getIntTuple3 #-}
getIntNTuples3V :: Int -> IO (V.Vector (Integer, Integer, Integer))
getIntNTuples3V !n = V.replicateM n getIntTuple3
{-# INLINE getIntNTuples3V #-}
getintGrid :: Int -> Int -> IO (IxVect (Int, Int) Int)
getintGrid !h !w = do
  !mv <- UM.new (h * w)
  forLoop 0 (== h) succ $ \ !r -> do
    !row <- BS.getLine
    let !rowOffset = r * w
        go !c !curS
          | c == w = return ()
          | otherwise = do
              let !trimmed = BS.dropWhile isSpace curS
              case BS.readInt trimmed of
                Just (!val, !rest) -> do
                  UM.unsafeWrite mv (rowOffset + c) val
                  go (c + 1) rest
                _ -> error "getintGrid : not Int"
    go 0 row
  IxVect ((0, 0), (h - 1, w - 1)) <$> U.unsafeFreeze mv
{-# INLINE getintGrid #-}
getGrid :: Int -> Int -> IO (IxVect (Int, Int) Char)
getGrid h w = do
  !rows <- replicateM (fromIntegral h) BS.getLine
  let !allBytes = BS.concat rows
      !vect = U.unfoldrN (h * w) BS.uncons allBytes
  return $ IxVect ((0, 0), (h - 1, w - 1)) vect
{-# INLINE getGrid #-}

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------
class Buildable a where
  toB :: a -> BSB.Builder

instance Buildable Int where 
  toB = BSB.intDec
  {-# INLINE toB #-}
instance Buildable Integer where 
  toB = BSB.integerDec
  {-# INLINE toB #-}
instance Buildable Char where 
  toB = BSB.char7
  {-# INLINE toB #-}
instance Buildable BS.ByteString where
  toB = BSB.byteString
  {-# INLINE toB #-}
instance Buildable String where
  toB = BSB.string7
  {-# INLINE toB #-}
instance (Buildable a, Buildable b) => Buildable (a, b) where
  toB (!x, !y) = toB x <> BSB.char7 ' ' <> toB y
  {-# INLINE toB #-}
instance (Buildable a, Buildable b, Buildable c) => Buildable (a, b, c) where
  toB (!x, !y, !z) = toB x <> BSB.char7 ' ' <> toB y <> BSB.char7 ' ' <> toB z
  {-# INLINE toB #-}
instance Buildable Bool where
  toB True = BSB.string7 "Yes"
  toB False = BSB.string7 "No"
  {-# INLINE toB #-}

putyn :: Bool -> IO ()
putyn = putLine
{-# INLINE putyn #-}
putYN :: Bool -> IO ()
putYN True  = BSB.hPutBuilder stdout $ BSB.string7 "YES\n"
putYN False = BSB.hPutBuilder stdout $ BSB.string7 "NO\n"
{-# INLINE putYN #-}

putLine :: Buildable a => a -> IO ()
putLine !x = BSB.hPutBuilder stdout (toB x <> BSB.char7 '\n')
{-# INLINE putLine #-}
putList :: Buildable a => [a] -> IO ()
putList [] = BSB.hPutBuilder stdout $ BSB.char7 '\n'
putList (!x : !xs) = do
  BSB.hPutBuilder stdout (toB x)
  mapM_ (\ !y -> BSB.hPutBuilder stdout (BSB.char7 ' ' <> toB y)) xs
  BSB.hPutBuilder stdout (BSB.char7 '\n')
{-# INLINE putList #-}
putU :: (U.Unbox a, Buildable a) => U.Vector a -> IO ()
putU !v = do
  if U.null v then BSB.hPutBuilder stdout $ BSB.char7 '\n'
  else do
    BSB.hPutBuilder stdout $ toB $ U.unsafeHead v
    U.forM_ (U.unsafeTail v) $ \ !x -> BSB.hPutBuilder stdout $ BSB.char7 ' ' <> toB x
    BSB.hPutBuilder stdout $ BSB.char7 '\n'
{-# INLINE putU #-}
putV :: Buildable a => V.Vector a -> IO ()
putV !v = do
  if V.null v then BSB.hPutBuilder stdout $ BSB.char7 '\n'
  else do
    BSB.hPutBuilder stdout $ toB $ V.unsafeHead v
    V.forM_ (V.unsafeTail v) $ \ !x -> BSB.hPutBuilder stdout $ BSB.char7 ' ' <> toB x
    BSB.hPutBuilder stdout $ BSB.char7 '\n'
{-# INLINE putV #-}
putLines :: Buildable a => [a] -> IO ()
putLines !xs = mapM_ (\ !x -> BSB.hPutBuilder stdout (toB x <> BSB.char7 '\n')) xs
{-# INLINE putLines #-}
putLinesU :: (U.Unbox a, Buildable a) => U.Vector a -> IO ()
putLinesU !v = U.forM_ v $ \ !x -> BSB.hPutBuilder stdout $ toB x <> BSB.char7 '\n'
{-# INLINE putLinesU #-}
putLinesV :: Buildable a => V.Vector a -> IO ()
putLinesV !v = V.forM_ v $ \ !x -> BSB.hPutBuilder stdout $ toB x <> BSB.char7 '\n'
{-# INLINE putLinesV #-}