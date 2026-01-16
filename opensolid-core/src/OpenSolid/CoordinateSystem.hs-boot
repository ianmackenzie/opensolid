module OpenSolid.CoordinateSystem (CoordinateSystem) where

import Data.Kind (Type)
import GHC.Num (Natural)

class CoordinateSystem (dimension :: Natural) (units :: Type) (space :: Type)
