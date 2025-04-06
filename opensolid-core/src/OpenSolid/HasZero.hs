module OpenSolid.HasZero (HasZero (zero)) where

import OpenSolid.Bootstrap

class HasZero a where
  zero :: a

instance HasZero Int where
  zero = 0
