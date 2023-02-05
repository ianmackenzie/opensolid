module Generic (Zero (..)) where

import Data.Coerce (coerce)
import OpenSolid

class Zero a where
  zero :: a

instance Zero (Qty units) where
  zero = coerce 0.0
