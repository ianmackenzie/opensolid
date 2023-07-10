module Sign (Sign (..)) where

import Arithmetic
import Prelude (Eq, Ord, Show)

data Sign = Negative | Positive deriving (Eq, Ord, Show)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign
