module Generic
  ( HasZero (zeroImpl)
  , zero
  , min
  , max
  , minBy
  , maxBy
  )
where

import Basics
import Prelude qualified

class HasZero a where
  zeroImpl :: a

instance HasZero Int where
  zeroImpl = 0

zero :: (HasZero a) => a
zero = zeroImpl

min :: (Ord a) => a -> a -> a
min = Prelude.min

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
