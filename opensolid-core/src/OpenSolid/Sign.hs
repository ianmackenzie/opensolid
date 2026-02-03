module OpenSolid.Sign
  ( Sign (Sign, Positive, Negative)
  , random
  , value
  )
where

import OpenSolid.Arithmetic
import {-# SOURCE #-} OpenSolid.Number (Number)
import {-# SOURCE #-} OpenSolid.Quantity (Quantity (Quantity))
import OpenSolid.Random.Internal qualified as Random
import System.Random qualified
import System.Random.Stateful qualified
import Prelude (Bounded, Enum, Eq, Ord, Show, (*))
import Prelude qualified

newtype Sign = Unit Number deriving (Eq, Ord, Show)

{-# COMPLETE Sign #-}

{-# INLINEABLE Sign #-}
pattern Sign :: Number -> Sign
pattern Sign x <- Unit x

{-# COMPLETE Negative, Positive #-}

{-# INLINEABLE Negative #-}
pattern Negative :: Sign
pattern Negative = Unit -1

{-# INLINEABLE Positive #-}
pattern Positive :: Sign
pattern Positive = Unit 1

instance Bounded Sign where
  minBound = Negative
  maxBound = Positive

instance Enum Sign where
  toEnum 0 = Negative
  toEnum _ = Positive
  fromEnum Negative = 0
  fromEnum Positive = 1

instance System.Random.Stateful.Uniform Sign where
  uniformM = System.Random.Stateful.uniformEnumM

instance Negation Sign where
  negative (Unit (Quantity x)) = Unit (Quantity -x)

instance Multiplication_ Sign Sign Sign where
  Unit (Quantity x) ?*? Unit (Quantity y) = Unit (Quantity (x * y))

instance Multiplication Sign Sign Sign where
  Unit (Quantity x) .*. Unit (Quantity y) = Unit (Quantity (x * y))

random :: Random.Generator Sign
random = Random.Generator System.Random.uniform

{-# INLINE value #-}
value :: Sign -> Number
value (Unit x) = x
