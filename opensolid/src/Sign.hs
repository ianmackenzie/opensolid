module Sign (Sign (Positive, Negative)) where

import Arithmetic
import Basics

data Sign = Negative | Positive deriving (Eq, Ord, Show)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication Sign Sign where
  type Sign .*. Sign = Sign
  Positive .*. sign = sign
  Negative .*. sign = -sign

instance Product Sign Sign Sign
