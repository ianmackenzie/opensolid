module OpenSolid.DirectionBounds3D (DirectionBounds3D, unsafe, unwrap) where

import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3D)

newtype DirectionBounds3D space
  = UnitBounds3D (VectorBounds3D Unitless space)

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3D space1) (DirectionBounds3D space2) (Interval Unitless)

unsafe :: VectorBounds3D Unitless space -> DirectionBounds3D space
unwrap :: DirectionBounds3D space -> VectorBounds3D Unitless space
