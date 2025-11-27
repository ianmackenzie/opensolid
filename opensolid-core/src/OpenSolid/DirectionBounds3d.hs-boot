module OpenSolid.DirectionBounds3d (DirectionBounds3d, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3d)

newtype DirectionBounds3d space
  = UnitBounds3d (VectorBounds3d Unitless space)

unsafe :: VectorBounds3d Unitless space -> DirectionBounds3d space
