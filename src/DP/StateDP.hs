{-# LANGUAGE LambdaCase #-}
module DP.StateDP where
import Control.Monad.State.Strict
import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HM
import Data.Vector.Unboxed qualified as U

type StateMemo s v = HM.HashMap s v
type StateDPMonad s v = State (StateMemo s v) v

_sdpMake :: Hashable s => ((s -> StateDPMonad s v) -> s -> StateDPMonad s v) -> (s -> StateDPMonad s v)
_sdpMake !transition = go
  where
    go !s = do
      gets (HM.lookup s) >>= \case
        Just !v -> return v
        Nothing -> do
          !v <- transition go s
          modify' (HM.insert s v)
          return v
{-# INLINE _sdpMake #-}

sdpRun :: Hashable s => ((s -> StateDPMonad s v) -> s -> StateDPMonad s v) -> s -> StateMemo s v -> v
sdpRun !transition !s = evalState (_sdpMake transition s)
{-# INLINE sdpRun #-}

sdpRunList :: Hashable s => ((s -> StateDPMonad s v) -> s -> StateDPMonad s v) -> [s] -> StateMemo s v -> [v]
sdpRunList !transition !ss = evalState (mapM (_sdpMake transition) ss)
{-# INLINE sdpRunList #-}

sdpRunVect :: (Hashable s, U.Unbox s, U.Unbox v) => ((s -> StateDPMonad s v) -> s -> StateDPMonad s v) -> U.Vector s -> StateMemo s v -> U.Vector v
sdpRunVect !transition !ss = evalState (U.mapM (_sdpMake transition) ss)
{-# INLINE sdpRunVect #-}

sdpRunWithMemo :: Hashable s => ((s -> StateDPMonad s v) -> s -> StateDPMonad s v) -> s -> StateMemo s v -> (v, StateMemo s v)
sdpRunWithMemo !transition !s = runState (_sdpMake transition s)
{-# INLINE sdpRunWithMemo #-}