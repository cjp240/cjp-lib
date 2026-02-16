{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Graph.Tree.HLD where
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.STRef
import Data.Tuple.Extra
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Data.MutableStack
import Graph.CSR

data HLDData = HLDData
  { hldPos :: !(U.Vector Int), -- vertex to index
    hldHead :: !(U.Vector Int), -- vertex to head vertex of path
    hldParent :: !(U.Vector Int), -- tree parent
    hldDepth :: !(U.Vector Int), -- vertex depth
    hldSubSize :: !(U.Vector Int), -- subtree size
    hldI2V :: !(U.Vector Int) -- index to vertex
  }

hlDecomp :: Int -> U.Vector (Int, Int) -> Int -> HLDData
hlDecomp !n !edges !root = runST $ do
  let !es = edges U.++ U.map swap edges
      !g = csrBuild n es
  parent <- UM.replicate n (-1)
  depth <- UM.replicate n 0
  stack <- msNew n
  order_ <- msNew n

  msPush stack root

  let fillOrder = msPop stack >>= \case
        Nothing -> return ()
        Just !v -> do
          msPush order_ v
          let !us = csrAdj g v
          !pv <- UM.unsafeRead parent v
          !dv <- UM.unsafeRead depth v
          U.forM_ us $ \ !u -> do
            when (u /= pv) do
              UM.unsafeWrite parent u v
              UM.unsafeWrite depth u $! dv + 1
              msPush stack u
          fillOrder

  fillOrder

  subSize <- UM.replicate n 1
  heavyChild <- UM.replicate n (-1)

  let go = msPop order_ >>= \case
        Nothing -> return ()
        Just !v -> do
          !pv <- UM.unsafeRead parent v
          when (pv /= -1) do
            !sv <- UM.unsafeRead subSize v
            UM.unsafeModify subSize (+ sv) pv

            !hcCurrent <- UM.unsafeRead heavyChild pv
            if hcCurrent == -1
              then UM.unsafeWrite heavyChild pv v
              else do
                !shc <- UM.unsafeRead subSize hcCurrent
                when (sv > shc) do UM.unsafeWrite heavyChild pv v
          go
  
  go

  pos <- UM.replicate n (-1)
  head_ <- UM.replicate n (-1)
  i2v <- UM.replicate n (-1)

  stack2 <- msNew n
  msPush stack2 (root, root)
  ptr <- newSTRef 0

  let buildPaths = msPop stack2 >>= \case
        Nothing -> return ()
        Just (!v, !h) -> do
          !p <- readSTRef ptr
          writeSTRef ptr $! p + 1
          UM.unsafeWrite pos v p
          UM.unsafeWrite i2v p v
          UM.unsafeWrite head_ v h

          !hc <- UM.unsafeRead heavyChild v
          !pv <- UM.unsafeRead parent v
          let !us = csrAdj g v

          U.forM_ us $ \ !u -> do
            when (u /= pv && u /= hc) do
              msPush stack2 (u, u)
          
          when (hc /= -1) do
            msPush stack2 (hc, h)

          buildPaths

  buildPaths
  
  HLDData 
    <$> U.unsafeFreeze pos
    <*> U.unsafeFreeze head_
    <*> U.unsafeFreeze parent
    <*> U.unsafeFreeze depth
    <*> U.unsafeFreeze subSize
    <*> U.unsafeFreeze i2v

hldPathFold :: HLDData -> Int -> Int -> a -> (a -> a -> a) -> ((Bool, Bool, Int, Int) -> a) -> a
hldPathFold HLDData{..} !u !v !mUnit !op !f =
  let go !currU !currV !accL !accR =
        let !hu = hldHead U.! currU
            !hv = hldHead U.! currV
        in 
          if hu == hv
            then
              let !pu = hldPos U.! currU
                  !pv = hldPos U.! currV
              in 
                if pu <= pv
                  then
                    let !vR = f (True, True, pu, pv + 1)
                        !accR' = op vR accR
                    in op accL accR'
                  else
                    let !vL = f (False, True, pv, pu + 1)
                        !accL' = op accL vL
                    in op accL' accR
            else
              let !dhu = hldDepth U.! hu
                  !dhv = hldDepth U.! hv
              in
                if dhu >= dhv
                  then
                    let !vL = f (False, False, hldPos U.! hu, hldPos U.! currU + 1)
                        !accL' = op accL vL
                    in go (hldParent U.! hu) currV accL' accR
                  else
                    let !vR = f (True, False, hldPos U.! hv, hldPos U.! currV + 1)
                        !accR' = op vR accR 
                    in go currU (hldParent U.! hv) accL accR'

  in go u v mUnit mUnit
{-# INLINE hldPathFold #-}

hldPathFoldM :: PrimMonad m => HLDData -> Int -> Int -> a -> (a -> a -> a) -> ((Bool, Bool, Int, Int) -> m a) -> m a
hldPathFoldM HLDData{..} !u !v !mUnit !op !f = do
  let go !currU !currV !accL !accR = do
        let !hu = hldHead U.! currU
            !hv = hldHead U.! currV
        if hu == hv
          then do
            let !pu = hldPos U.! currU
                !pv = hldPos U.! currV
            if pu <= pv
              then do
                !vR <- f (True, True, pu, pv + 1)
                let !accR' = op vR accR
                return $! op accL accR'
              else do
                !vL <- f (False, True, pv, pu + 1)
                let !accL' = op accL vL
                return $! op accL' accR
          else do
            let !dhu = hldDepth U.! hu
                !dhv = hldDepth U.! hv
            if dhu >= dhv
              then do
                !vL <- f (False, False, hldPos U.! hu, hldPos U.! currU + 1)
                let !accL' = op accL vL
                go (hldParent U.! hu) currV accL' accR
              else do
                !vR <- f (True, False, hldPos U.! hv, hldPos U.! currV + 1)
                let !accR' = op vR accR 
                go currU (hldParent U.! hv) accL accR'

  go u v mUnit mUnit
{-# INLINE hldPathFoldM #-}

hldSubtree :: HLDData -> Int -> (Int, Int)
hldSubtree HLDData{..} !v = 
  let !l = hldPos U.! v
      !r = l + hldSubSize U.! v
  in (l, r)
{-# INLINE hldSubtree #-}

hldLCA :: HLDData -> Int -> Int -> Int
hldLCA HLDData{..} !u !v = go u v
  where
    go !currU !currV = 
      let !hu = hldHead U.! currU
          !hv = hldHead U.! currV
      in if hu == hv
         then if hldDepth U.! currU < hldDepth U.! currV then currU else currV
         else 
          let !dhu = hldDepth U.! hu
              !dhv = hldDepth U.! hv
          in if dhu > dhv
             then go (hldParent U.! hu) currV
             else go currU (hldParent U.! hv)
{-# INLINE hldLCA #-}

hldDeeper :: HLDData -> Int -> Int -> Int
hldDeeper HLDData{..} !u !v =
  if (hldDepth U.! u) > (hldDepth U.! v) then u else v
{-# INLINE hldDeeper #-}