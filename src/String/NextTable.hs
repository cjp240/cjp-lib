module String.NextTable where
import Control.Monad.ST
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.IxVector
import Common.Template

nexBuild :: Int -> U.Vector Int -> IxVect (Int, Int) Int
nexBuild !szAlphabet !s = runST $ do
  let !n = U.length s
  !nex <- UM.replicate ((n + 1) * szAlphabet) n
  forLoop (n - 1) (< 0) pred $ \ !i -> do
    let !currOffset = i * szAlphabet
        !nextOffset = (i + 1) * szAlphabet
    UM.copy (UM.unsafeSlice currOffset szAlphabet nex) (UM.unsafeSlice nextOffset szAlphabet nex)
    let !c = U.unsafeIndex s i
    UM.unsafeWrite nex (currOffset + c) i
  IxVect ((0, 0), (n, szAlphabet - 1)) <$> U.unsafeFreeze nex