module Generic
  ( HasZero (zero)
  , min
  , max
  , minBy
  , maxBy
  )
where

import Basics
import Prelude qualified

class HasZero a where
  zero :: a

instance HasZero Int where
  zero = 0

{-# INLINE min #-}
min :: (Ord a) => a -> a -> a
min = Prelude.min

{-# INLINE max #-}
max :: (Ord a) => a -> a -> a
max = Prelude.max

minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy function first second
  | function first <= function second = first
  | otherwise = second

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy function first second
  | function first >= function second = first
  | otherwise = second
