module VectorCurve3d
  ( VectorCurve3d
  , constant
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Units qualified
import Vector3d (Vector3d)

type role VectorCurve3d nominal

data VectorCurve3d (coordinateSystem :: CoordinateSystem)

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)

instance
  (Units.Product units1 units2 units3) =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance
  (Units.Product units1 units2 units3) =>
  Multiplication (Curve1d units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
