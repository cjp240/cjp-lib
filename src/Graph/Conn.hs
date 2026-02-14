{-# LANGUAGE RecordWildCards #-}
module Graph.Conn where
import Data.Vector.Unboxed qualified as U
import Graph.Conn.LowLink
import Graph.CSR

getArticulations :: LowLink -> U.Vector Int
getArticulations ll@LowLink{..} = U.filter (isArticulation ll) $ U.enumFromN 0 llN
{-# INLINE getArticulations #-}

isArticulation :: LowLink -> Int -> Bool
isArticulation LowLink{..} !u
  | U.unsafeIndex llParent u == -1 = U.unsafeIndex llTreeChildCnt u >= 2
  | otherwise = 
      let !vs = csrAdj llCSR u
          isFatal !v = U.unsafeIndex llParent v == u && U.unsafeIndex llLow v >= U.unsafeIndex llOrd u
      in U.any (isFatal . fst) vs
{-# INLINE isArticulation #-}

getBridges :: LowLink -> U.Vector Int
getBridges ll@LowLink{..} = U.map fst $ U.filter (uncurry (isBridge ll) . snd) $ U.indexed llEdges
{-# INLINE getBridges #-}

isBridge :: LowLink -> Int -> Int -> Bool
isBridge LowLink{..} !u !v = 
  let !ou = U.unsafeIndex llOrd u
      !ov = U.unsafeIndex llOrd v
      !lu = U.unsafeIndex llLow u
      !lv = U.unsafeIndex llLow v
  in if ou < ov then lv > ou else lu > ov
{-# INLINE isBridge #-}