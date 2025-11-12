module OpenSolid.DirectionBounds3d (DirectionBounds3d, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3d)

newtype DirectionBounds3d space
  = UnitBounds3d (VectorBounds3d (space @ Unitless))

unsafe :: VectorBounds3d (space @ Unitless) -> DirectionBounds3d space
