module Sign (Sign (..)) where

import Arithmetic
import Prelude (Eq, Ord, Show)

data Sign = Negative | Positive deriving (Eq, Ord, Show)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance {-# OVERLAPS #-} Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign

instance Negation a => Multiplication Sign a a where
  Positive * value = value
  Negative * value = -value

instance Negation a => Multiplication a Sign a where
  value * Positive = value
  value * Negative = -value
