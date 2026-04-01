module OpenSolid.VectorBounds (VectorBounds, Exists) where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds3D (VectorBounds3D)

type family
  VectorBounds dimension units space =
    vectorBounds | vectorBounds -> dimension units space
  where
  VectorBounds 1 units Void = Interval units
  VectorBounds 2 units space = VectorBounds2D units space
  VectorBounds 3 units space = VectorBounds3D units space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)
