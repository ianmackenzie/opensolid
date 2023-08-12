module Generic
  ( Zero (zero)
  , min
  , max
  , minBy
  , maxBy
  , Ordering (LT, EQ, GT)
  , compare
  )
where

import Basics
import Prelude (Ordering (EQ, GT, LT))
import Prelude qualified

class Zero a where
  zero :: a

instance Zero Int where
  zero = 0

min :: Ord a => a -> a -> a
min = Prelude.min

max :: Ord a => a -> a -> a
max = Prelude.max

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy function first second
  | function first Prelude.<= function second = first
  | otherwise = second

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy function first second
  | function first Prelude.>= function second = first
  | otherwise = second

compare :: Ord a => a -> a -> Ordering
compare = Prelude.compare
