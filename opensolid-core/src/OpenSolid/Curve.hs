module OpenSolid.Curve
  ( IsPoint (IsPoint)
  , derivative
  , tangentDirection
  , findPoint
  )
where

import OpenSolid.CoordinateSystem
  ( CoordinateSystem (Curve, Point)
  , VectorCoordinateSystem (DirectionCurve, VectorCurve)
  )
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude
import OpenSolid.VectorCurve qualified as VectorCurve

data IsPoint = IsPoint deriving (Eq, Show)

derivative ::
  CoordinateSystem dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
derivative = CoordinateSystem.curveDerivative

tangentDirection ::
  (CoordinateSystem dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (DirectionCurve dimension space)
tangentDirection curve =
  case VectorCurve.direction (derivative curve) of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve.IsZero -> Error IsPoint

findPoint ::
  (CoordinateSystem dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Result IsPoint (List Number)
findPoint point curve =
  case VectorCurve.zeros (point .-. curve) of
    Error VectorCurve.IsZero -> Error IsPoint
    Ok parameterValues -> Ok parameterValues
