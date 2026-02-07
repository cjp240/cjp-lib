{-# LANGUAGE RecordWildCards #-}
module String.Trie where
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

data Trie m a = Trie
  { trieNxt :: !(U.MVector (PrimState m) Int),
    trieVal :: !(U.MVector (PrimState m) a),
    triePrefix :: !(UM.MVector (PrimState m) a),
    trieCnt :: !(UM.MVector (PrimState m) Int),
    trieSzAlphabet :: !Int,
    trieNodeCnt :: !(MutVar (PrimState m) Int),
    trieOp :: !(a -> a -> a),
    trieUnit :: !a,

    trieDFA :: !(UM.MVector (PrimState m) Int),
    trieFail :: !(UM.MVector (PrimState m) Int),
    trieDictLink :: !(UM.MVector (PrimState m) Int),
    trieSum :: !(UM.MVector (PrimState m) a)
  }

trieNew :: (PrimMonad m, UM.Unbox a) => Int -> Int -> (a -> a -> a) -> a -> m (Trie m a)
trieNew !maxN !sz !op !tUnit = do
  nxt <- UM.replicate ((maxN + 1) * sz) (-1)
  val <- UM.replicate (maxN + 1) tUnit
  pre <- UM.replicate (maxN + 1) tUnit
  cnt <- UM.replicate (maxN + 1) 0
  nodeCnt <- newMutVar 1

  dfa <- UM.replicate ((maxN + 1) * sz) (-1)
  tFail <- UM.replicate (maxN + 1) 0
  dictLink <- UM.replicate (maxN + 1) 0
  tSum <- UM.replicate (maxN + 1) tUnit
  
  return $ Trie nxt val pre cnt sz nodeCnt op tUnit dfa tFail dictLink tSum

trieGetTransition :: PrimMonad m => Trie m a -> Int -> Int -> m (Maybe Int)
trieGetTransition Trie{..} !v !c = do
  let !idx = v * trieSzAlphabet + c
  !nxt <- UM.unsafeRead trieNxt idx
  if nxt == -1
    then return Nothing
    else return $ Just nxt
{-# INLINE trieGetTransition #-}

trieInsert :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> a -> m Int
trieInsert Trie{..} !w !v = do
  !finalNode <-
    U.foldM'
      (\ !curr !c -> do
        UM.unsafeModify triePrefix (`trieOp` v) curr
        let !idx = curr * trieSzAlphabet + c
        !nxt <- UM.unsafeRead trieNxt idx
        if nxt == -1
          then do
            !newNode <- readMutVar trieNodeCnt
            writeMutVar trieNodeCnt $! newNode + 1
            UM.unsafeWrite trieNxt idx newNode
            return newNode
          else return nxt
      )
      0 w
  UM.unsafeModify triePrefix (`trieOp` v) finalNode
  UM.unsafeModify trieVal (`trieOp` v) finalNode
  UM.unsafeModify trieCnt succ finalNode
  return finalNode
{-# INLINABLE trieInsert #-}

trieInsert_ :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> a -> m ()
trieInsert_ !t !w !v = void $ trieInsert t w v
{-# INLINE trieInsert_ #-}

trieDelete :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> a -> m ()
trieDelete t@Trie{..} !w !invV = do
  let !n = U.length w
      go !curr !i
        | i == n = do
            UM.unsafeModify triePrefix (`trieOp` invV) curr
            UM.unsafeModify trieVal (`trieOp` invV) curr
            !currCnt <- UM.unsafeRead trieCnt curr
            UM.unsafeWrite trieCnt curr $! max 0 (currCnt - 1)
        | otherwise = do
            let !c = U.unsafeIndex w i
            UM.unsafeModify triePrefix (`trieOp` invV) curr
            !nxt <- trieGetTransition t curr c
            case nxt of
              Nothing -> return ()
              Just !nxtV -> go nxtV (i + 1)

  go 0 0
{-# INLINABLE trieDelete #-}

trieGetVal :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> m a
trieGetVal !t !w = _trieGet t (trieVal t) w
{-# INLINE trieGetVal #-}

trieGetPrefixVal :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> m a
trieGetPrefixVal !t !w = _trieGet t (triePrefix t) w
{-# INLINE trieGetPrefixVal #-}

_trieGet :: (PrimMonad m, UM.Unbox a) => Trie m a -> UM.MVector (PrimState m) a -> U.Vector Int -> m a
_trieGet Trie{..} !mv !w = do
  let !len = U.length w
      go !curr !i
        | i == len = UM.unsafeRead mv curr
        | otherwise = do
            let !c = U.unsafeIndex w i
                !idx = curr * trieSzAlphabet + c
            !nxt <- UM.unsafeRead trieNxt idx
            if nxt == -1
              then return trieUnit
              else go nxt (i + 1)
  go 0 0
{-# INLINABLE _trieGet #-}