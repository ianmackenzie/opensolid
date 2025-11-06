module OpenSolid.HasZero (HasZero (zero)) where

import Prelude (Int)

class HasZero a where
  zero :: a

instance HasZero Int where
  zero = 0
