{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Common.Debug where
import Data.List qualified as L
import Data.Vector.Unboxed qualified as U
import Debug.Trace

import Common.IxVector

#ifndef ATCODER
-- ローカル環境
dbg :: Show a => a -> ()
dbg = (`traceShow` ())

dbgGrid :: (Show e, U.Unbox e) => IxVect (Int, Int) e -> ()
dbgGrid IxVect{..} = L.foldl' step () rows
  where
    ((_, !yl), (_, !yr)) = bndsIV
    !w = yr - yl + 1
    rows = L.unfoldr f (U.toList vectIV)
      where
        f [] = Nothing
        f xs = Just (L.splitAt w xs)
    step () = (`trace` ()) . L.unwords . L.map (align . show)
    align s = L.replicate (max 0 (5 - L.length s)) ' ' ++ s

#else
-- AtCoder 環境
dbg :: Show a => a -> ()
dbg = const ()

dbgGrid :: (Show e, U.Unbox e) => IxVect (Int, Int) e -> ()
dbgGrid = const ()

#endif