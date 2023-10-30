module Sign (Sign (Positive, Negative)) where

import Arithmetic
import Basics

data Sign = Negative | Positive deriving (Eq, Ord, Show)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign
