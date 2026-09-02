module OpenSolid.Zero (Zero (zero)) where

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

class Zero a where
  zero :: a

instance Zero Int where
  zero = 0

instance Zero (Quantity units) where
  zero = Quantity.zero
