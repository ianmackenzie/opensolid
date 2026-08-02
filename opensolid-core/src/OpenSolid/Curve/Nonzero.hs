module OpenSolid.Curve.Nonzero
  ( point
  , derivative
  , tangentDirectionValue
  , tangentDirectionRange
  , curvatureVectorValue
  , curvatureVectorRange
  , curvatureVectorValue_
  , curvatureVectorRange_
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)

point :: Nonzero (Curve dimension units space) -> Number -> Point dimension units space
point (Nonzero curve) parameterValue = Curve.point curve parameterValue

derivative :: Nonzero (Curve dimension units space) -> Nonzero (VectorCurve dimension units space)
derivative (Nonzero curve) = Nonzero (Curve.derivative curve)

tangentDirectionValue ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Nonzero (Curve dimension units space) ->
  Number ->
  Direction dimension space
tangentDirectionValue (Nonzero curve) tValue = do
  let derivativeValue = Curve.derivativeValue curve tValue
  Direction.unsafe (derivativeValue / Vector.magnitude derivativeValue)

tangentDirectionRange ::
  (Curve.Exists dimension units space, DirectionBounds.Exists dimension space) =>
  Nonzero (Curve dimension units space) ->
  Interval Unitless ->
  DirectionBounds dimension space
tangentDirectionRange (Nonzero curve) tRange =
  VectorBounds.direction (Curve.derivativeRange curve tRange)

curvatureVectorValue ::
  ( Curve.Exists dimension units space
  , Units.Inverse units inverseUnits
  , Vector.Exists dimension inverseUnits space
  ) =>
  Nonzero (Curve dimension units space) ->
  Number ->
  Vector dimension inverseUnits space
curvatureVectorValue curve tValue = Vector.coerce (curvatureVectorValue_ curve tValue)

curvatureVectorRange ::
  ( Curve.Exists dimension units space
  , Units.Inverse units inverseUnits
  , VectorBounds.Exists dimension inverseUnits space
  ) =>
  Nonzero (Curve dimension units space) ->
  Interval Unitless ->
  VectorBounds dimension inverseUnits space
curvatureVectorRange curve tRange = VectorBounds.coerce (curvatureVectorRange_ curve tRange)

curvatureVectorValue_ ::
  (Curve.Exists dimension units space, Vector.Exists dimension (Unitless ?/? units) space) =>
  Nonzero (Curve dimension units space) ->
  Number ->
  Vector dimension (Unitless ?/? units) space
curvatureVectorValue_ (Nonzero curve) tValue =
  Curve.CurvatureVector.value_
    (Curve.derivativeValue curve tValue)
    (Curve.secondDerivativeValue curve tValue)

curvatureVectorRange_ ::
  (Curve.Exists dimension units space, VectorBounds.Exists dimension (Unitless ?/? units) space) =>
  Nonzero (Curve dimension units space) ->
  Interval Unitless ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorRange_ (Nonzero curve) tRange =
  Curve.CurvatureVector.range_
    (Curve.derivativeRange curve tRange)
    (Curve.secondDerivativeRange curve tRange)
