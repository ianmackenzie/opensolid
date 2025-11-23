module OpenSolid.DirectionBounds2d (DirectionBounds2d, unsafe) where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2d)

newtype DirectionBounds2d space
  = UnitBounds2d (VectorBounds2d space Unitless)

unsafe :: VectorBounds2d space Unitless -> DirectionBounds2d space
