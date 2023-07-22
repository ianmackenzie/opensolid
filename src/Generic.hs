module Generic
  ( Zero (zero)
  , min
  , max
  , Ordering (LT, EQ, GT)
  , compare
  )
where

import Prelude (Ord, Ordering (EQ, GT, LT))
import Prelude qualified

class Zero a where
  zero :: a

min :: Ord a => a -> a -> a
min = Prelude.min

max :: Ord a => a -> a -> a
max = Prelude.max

compare :: Ord a => a -> a -> Ordering
compare = Prelude.compare
