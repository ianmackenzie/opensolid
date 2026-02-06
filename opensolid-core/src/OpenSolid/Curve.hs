module OpenSolid.Curve
  ( Curve
  , Exists
  , IsPoint (IsPoint)
  , derivative
  , bounds
  , evaluate
  , evaluateBounds
  , secondDerivative
  , tangentDirection
  , findPoint
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

data IsPoint = IsPoint deriving (Eq, Show)

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  ) =>
  Exists dimension units space
  where
  derivative :: Curve dimension units space -> VectorCurve dimension units space
  bounds :: Curve dimension units space -> Bounds dimension units space
  evaluate :: Curve dimension units space -> Number -> Point dimension units space
  evaluateBounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space

instance Exists 2 units space where
  derivative = Curve2D.derivative
  bounds = Curve2D.bounds
  evaluate = Curve2D.evaluate
  evaluateBounds = Curve2D.evaluateBounds

instance Exists 3 Meters space where
  derivative = Curve3D.derivative
  bounds = Curve3D.bounds
  evaluate = Curve3D.evaluate
  evaluateBounds = Curve3D.evaluateBounds

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (DirectionCurve dimension space)
tangentDirection curve =
  case VectorCurve.direction (derivative curve) of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve.IsZero -> Error IsPoint

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Result IsPoint (List Number)
findPoint point curve =
  case VectorCurve.zeros (point .-. curve) of
    Error VectorCurve.IsZero -> Error IsPoint
    Ok parameterValues -> Ok parameterValues
