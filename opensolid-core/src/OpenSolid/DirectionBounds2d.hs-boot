module OpenSolid.DirectionBounds2d (DirectionBounds2d, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2d)

newtype DirectionBounds2d space
  = UnitBounds2d (VectorBounds2d Unitless space)

unsafe :: VectorBounds2d Unitless space -> DirectionBounds2d space
