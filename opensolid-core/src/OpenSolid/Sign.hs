module OpenSolid.Sign
  ( Sign (Positive, Negative)
  , random
  )
where

import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import OpenSolid.Random.Internal qualified as Random
import System.Random qualified
import System.Random.Stateful qualified
import Prelude qualified

data Sign = Negative | Positive deriving (Eq, Ord, Show, Prelude.Enum, Prelude.Bounded)

instance System.Random.Stateful.Uniform Sign where
  uniformM = System.Random.Stateful.uniformEnumM

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Multiplication' Sign Sign Sign where
  Positive .*. sign = sign
  Negative .*. sign = -sign

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign

instance Exponentiation Sign Int where
  Positive ** _ = Positive
  Negative ** n = if Prelude.even n then Positive else Negative

random :: Random.Generator Sign
random = Random.Generator System.Random.uniform
