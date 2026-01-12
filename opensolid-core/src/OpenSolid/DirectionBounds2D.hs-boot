module OpenSolid.DirectionBounds2D (DirectionBounds2D, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2D)

newtype DirectionBounds2D space
  = UnitBounds2D (VectorBounds2D Unitless space)

unsafe :: VectorBounds2D Unitless space -> DirectionBounds2D space
