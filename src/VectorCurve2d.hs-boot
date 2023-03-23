module VectorCurve2d
  ( VectorCurve2d
  , constant
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Units qualified
import Vector2d (Vector2d)

type role VectorCurve2d nominal

data VectorCurve2d (coordinateSystem :: CoordinateSystem)

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))
