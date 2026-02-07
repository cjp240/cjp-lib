{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module String.Trie.AhoCorasick where
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Common.Template
import Data.MutableQueue
import String.Trie

acBuild :: (PrimMonad m, UM.Unbox a) => Trie m a -> m (U.Vector Int)
acBuild Trie{..} = do
  !nSz <- readMutVar trieNodeCnt
  que <- mqNew nSz

  forLoop 0 (== trieSzAlphabet) succ $ \ !c -> do
    !nxt <- UM.unsafeRead trieNxt c
    case nxt of
      -1 -> do
        UM.unsafeWrite trieDFA c 0
      _ -> do
        UM.unsafeWrite trieDFA c nxt
        UM.unsafeWrite trieFail nxt 0
        !vNxt <- UM.unsafeRead trieVal nxt
        UM.unsafeWrite trieSum nxt vNxt
        mqPush que c

  let bfs = mqPop que >>= \case
        Nothing -> return ()
        Just !u -> do
          !fu <- UM.unsafeRead trieFail u
          let !offset = u * trieSzAlphabet
              !offsetF = fu * trieSzAlphabet
          
          forLoop 0 (== trieSzAlphabet) succ $ \ !c -> do
            let !idx = offset + c
                !idxF = offsetF + c
            !fv <- UM.unsafeRead trieDFA idxF
            !nxt <- UM.unsafeRead trieNxt idx
            case nxt of
              -1 -> do
                UM.unsafeWrite trieDFA idx fv
              _ -> do
                !fvCnt <- UM.unsafeRead trieCnt fv
                if fvCnt > 0
                  then UM.unsafeWrite trieDictLink nxt fv
                  else do
                    !dlFv <- UM.unsafeRead trieDictLink fv
                    UM.unsafeWrite trieDictLink nxt dlFv
                
                UM.unsafeWrite trieFail nxt fv
                !vNxt <- UM.unsafeRead trieVal nxt
                !sumFv <- UM.unsafeRead trieSum fv
                UM.unsafeWrite trieSum nxt $! trieOp sumFv vNxt

                UM.unsafeWrite trieDFA idx nxt
                mqPush que nxt

          bfs

  bfs
  mqUnsafeFreeze que
{-# INLINABLE acBuild #-}

acBuild_ :: (PrimMonad m, UM.Unbox a) => Trie m a -> m ()
acBuild_ = void . acBuild
{-# INLINE acBuild_ #-}

acGetTransition :: PrimMonad m => Trie m a -> Int -> Int -> m Int
acGetTransition Trie{..} !v !c = do
  let !idx = v * trieSzAlphabet + c
  UM.unsafeRead trieDFA idx
{-# INLINE acGetTransition #-}

acGetSum :: (PrimMonad m, UM.Unbox a) => Trie m a -> U.Vector Int -> m a
acGetSum t@Trie{..} !w = do
  !finalNode <- U.foldM' (acGetTransition t) 0 w
  UM.unsafeRead trieSum finalNode
{-# INLINE acGetSum #-}