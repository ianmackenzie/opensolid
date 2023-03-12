module Sign (Sign (..)) where

import Arithmetic
import Prelude (Eq, Show)

data Sign = Positive | Negative deriving (Eq, Show)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign
