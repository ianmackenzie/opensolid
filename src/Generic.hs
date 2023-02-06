module Generic
  ( Zero (..)
  , Units
  , SquaredUnits
  )
where

import Data.Coerce (coerce)
import OpenSolid

class Zero a where
  zero :: a

instance Zero (Qty units) where
  zero = coerce 0.0

data Units

data SquaredUnits

instance IsProduct (Qty Units) (Qty Units) (Qty SquaredUnits)

instance IsQuotient (Qty SquaredUnits) (Qty Units) (Qty Units)

instance Squared (Qty Units) (Qty SquaredUnits)
