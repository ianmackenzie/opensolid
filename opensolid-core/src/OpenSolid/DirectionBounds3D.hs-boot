module OpenSolid.DirectionBounds3D (DirectionBounds3D, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3D)

newtype DirectionBounds3D space
  = UnitBounds3D (VectorBounds3D Unitless space)

unsafe :: VectorBounds3D Unitless space -> DirectionBounds3D space
