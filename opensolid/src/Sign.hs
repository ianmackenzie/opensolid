module Sign (Sign (Positive, Negative)) where

import Arithmetic
import Basics
import System.Random.Stateful qualified
import Prelude qualified

data Sign = Negative | Positive deriving (Eq, Ord, Show, Prelude.Enum, Prelude.Bounded)

instance System.Random.Stateful.Uniform Sign where
  uniformM = System.Random.Stateful.uniformEnumM

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication' Sign Sign where
  type Sign .*. Sign = Sign
  Positive .*. sign = sign
  Negative .*. sign = -sign

instance Multiplication Sign Sign Sign

instance Exponentiation Sign Int where
  Positive ** _ = Positive
  Negative ** n = if Prelude.even n then Positive else Negative
