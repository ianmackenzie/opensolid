module OpenSolid.DirectionBounds2D (DirectionBounds2D, unsafe, unwrap) where

import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2D)

newtype DirectionBounds2D = UnitBounds2D (VectorBounds2D Unitless)

instance DotMultiplication DirectionBounds2D DirectionBounds2D (Interval Unitless)

unsafe :: VectorBounds2D Unitless -> DirectionBounds2D
unwrap :: DirectionBounds2D -> VectorBounds2D Unitless
