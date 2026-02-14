module OpenSolid.DirectionBounds2D (DirectionBounds2D, unsafe, unwrap) where

import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2D)

newtype DirectionBounds2D space
  = UnitBounds2D (VectorBounds2D Unitless space)

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2D space1) (DirectionBounds2D space2) (Interval Unitless)

unsafe :: VectorBounds2D Unitless space -> DirectionBounds2D space
unwrap :: DirectionBounds2D space -> VectorBounds2D Unitless space
