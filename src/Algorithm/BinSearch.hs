module Algorithm.BinSearch where

--------------------------------------------------------------------------------
-- Integral
--------------------------------------------------------------------------------
binSearchMin :: Integral t => (t -> Bool) -> t -> t -> t
binSearchMin f !left !right = step left right
  where
    step !l !r
      | r - l == 1 = r
      | f m = step l m
      | otherwise = step m r
      where
        !m = l + div (r - l) 2
{-# INLINE binSearchMin #-}

binSearchMax :: Integral t => (t -> Bool) -> t -> t -> t
binSearchMax f !left !right = step left right
  where
    step !l !r
      | r - l == 1 = l
      | f m = step m r
      | otherwise = step l m
      where
        !m = l + div (r - l) 2
{-# INLINE binSearchMax #-}

--------------------------------------------------------------------------------
-- Double
--------------------------------------------------------------------------------
binSearchMinD :: (Double -> Bool) -> Double -> Double -> Double
binSearchMinD !f !left !right = step (100 :: Int) left right
  where
    step 0 _ !r = r
    step !i !l !r
      | f m = step (i - 1) l m
      | otherwise = step (i - 1) m r
      where
        !m = (l + r) / 2
{-# INLINE binSearchMinD #-}

binSearchMaxD :: (Double -> Bool) -> Double -> Double -> Double
binSearchMaxD !f !left !right = step (100 :: Int) left right
  where
    step 0 !l _ = l
    step !i !l !r
      | f m = step (i - 1) m r
      | otherwise = step (i - 1) l m
      where
        !m = (l + r) / 2
{-# INLINE binSearchMaxD #-}

--------------------------------------------------------------------------------
-- Monadic, Integral
--------------------------------------------------------------------------------
binSearchMinM :: (Monad m, Integral t) => (t -> m Bool) -> t -> t -> m t
binSearchMinM f !left !right = step left right
  where
    step !l !r = do
      if r - l == 1 then return r
      else do
        let !m = l + div (r - l) 2
        !t <- f m
        if t then step l m
        else step m r
{-# INLINE binSearchMinM #-}

binSearchMaxM :: (Monad m, Integral t) => (t -> m Bool) -> t -> t -> m t
binSearchMaxM f !left !right = step left right
  where
    step !l !r = do
      if r - l <= 1 then return l
      else do
        let !m = l + div (r - l) 2
        !t <- f m
        if t then step m r
        else step l m
{-# INLINE binSearchMaxM #-}

--------------------------------------------------------------------------------
-- Monadic, Double
--------------------------------------------------------------------------------
binSearchMinMD :: Monad m => (Double -> m Bool) -> Double -> Double -> m Double
binSearchMinMD f !left !right = step (100 :: Int) left right
  where
    step 0 _ !r = return r
    step !i !l !r = do
      let !m = (l + r) / 2
      !t <- f m
      if t then step (i - 1) l m
      else step (i - 1) m r
{-# INLINE binSearchMinMD #-}

binSearchMaxMD :: Monad m => (Double -> m Bool) -> Double -> Double -> m Double
binSearchMaxMD f !left !right = step (100 :: Int) left right
  where
    step 0 !l _ = return l
    step !i !l !r = do
      let !m = (l + r) / 2
      !t <- f m
      if t then step (i - 1) m r
      else step (i - 1) l m
{-# INLINE binSearchMaxMD #-}