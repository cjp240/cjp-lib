{-# LANGUAGE BlockArguments #-}
module Algorithm.TwoSAT where
import Data.Bits
import Data.Vector.Unboxed qualified as U

import Graph.Conn.SCC

newtype SATLit = SATLit {unSATLit :: Int} deriving (Eq, Show)

satMkLit :: Int -> Bool -> SATLit
satMkLit !i True = SATLit (2 * i)
satMkLit !i False = SATLit (2 * i + 1)
{-# INLINE satMkLit #-}

satNot :: SATLit -> SATLit
satNot (SATLit !l) = SATLit (xor l 1)
{-# INLINE satNot #-}

-- A v B
satOr :: SATLit -> SATLit -> [(Int, Int)]
satOr !a !b = [(unSATLit (satNot a), unSATLit b), (unSATLit (satNot b), unSATLit a)]
{-# INLINE satOr #-}

-- A => B
satImply :: SATLit -> SATLit -> [(Int, Int)]
satImply !a !b = satOr (satNot a) b
{-# INLINE satImply #-}

-- A <=> B
satEquiv :: SATLit -> SATLit -> [(Int, Int)]
satEquiv !a !b = satImply a b ++ satImply b a
{-# INLINE satEquiv #-}

-- A xor B (A /= B)
satXor :: SATLit -> SATLit -> [(Int, Int)]
satXor !a !b = satOr a b ++ satOr (satNot a) (satNot b)
{-# INLINE satXor #-}

-- a :: True
satSetTrue :: SATLit -> [(Int, Int)]
satSetTrue !a = satOr a a
{-# INLINE satSetTrue #-}

-- a :: False
satSetFalse :: SATLit -> [(Int, Int)]
satSetFalse !a = satOr (satNot a) (satNot a)
{-# INLINE satSetFalse #-}

satSolve :: Int -> U.Vector (Int, Int) -> Maybe (U.Vector Bool)
satSolve !n !clause
  | hasContradiction = Nothing
  | otherwise = Just $ U.generate n $ \ !i -> 
      let !idTrue = cmp U.! (2 * i)
          !idFalse = cmp U.! (2 * i + 1)
      in idTrue > idFalse
  where
    (_, !cmp, _, _) = sccDecomp (2 * n) clause
    !hasContradiction = U.any (\i -> cmp U.! (2 * i) == cmp U.! (2 * i + 1)) $ U.enumFromN 0 n
{-# INLINE satSolve #-}