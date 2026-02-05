module OpenSolid.HasZero (HasZero (zero)) where

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

class HasZero a where
  zero :: a

instance HasZero Int where
  zero = 0

instance HasZero (Quantity units) where
  zero = Quantity.zero
