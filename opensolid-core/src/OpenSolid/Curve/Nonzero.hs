module OpenSolid.Curve.Nonzero
  ( pointAt
  , pointOn
  , derivative
  , derivativeAt
  , tangentDirectionAt
  , tangentDirectionRange
  , curvatureVectorAt
  , curvatureVectorRange
  , curvatureVectorAt_
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
import OpenSolid.VectorCurve.Nonzero qualified as VectorCurve.Nonzero

pointAt :: Number -> Nonzero (Curve dimension units space) -> Point dimension units space
pointAt tValue (Nonzero curve) = Curve.pointAt tValue curve

pointOn :: Nonzero (Curve dimension units space) -> Number -> Point dimension units space
pointOn curve tValue = pointAt tValue curve

derivative :: Nonzero (Curve dimension units space) -> Nonzero (VectorCurve dimension units space)
derivative (Nonzero curve) = Nonzero (Curve.derivative curve)

derivativeAt ::
  (Curve.Exists dimension units space, Vector.Exists dimension units space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Nonzero (Vector dimension units space)
derivativeAt tValue curve = VectorCurve.Nonzero.valueAt tValue (derivative curve)

tangentDirectionAt ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Direction dimension space
tangentDirectionAt tValue (Nonzero curve) = do
  let derivativeValue = Curve.derivativeAt tValue curve
  Direction.unsafe (derivativeValue / Vector.magnitude derivativeValue)

tangentDirectionRange ::
  (Curve.Exists dimension units space, DirectionBounds.Exists dimension space) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  DirectionBounds dimension space
tangentDirectionRange tRange (Nonzero curve) =
  VectorBounds.direction (Curve.derivativeRange tRange curve)

curvatureVectorAt ::
  ( Curve.Exists dimension units space
  , Units.Inverse units inverseUnits
  , Vector.Exists dimension inverseUnits space
  ) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Vector dimension inverseUnits space
curvatureVectorAt tValue curve = Vector.coerce (curvatureVectorAt_ tValue curve)

curvatureVectorRange ::
  ( Curve.Exists dimension units space
  , Units.Inverse units inverseUnits
  , VectorBounds.Exists dimension inverseUnits space
  ) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  VectorBounds dimension inverseUnits space
curvatureVectorRange tRange curve = VectorBounds.coerce (curvatureVectorRange_ tRange curve)

curvatureVectorAt_ ::
  (Curve.Exists dimension units space, Vector.Exists dimension (Unitless ?/? units) space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Vector dimension (Unitless ?/? units) space
curvatureVectorAt_ tValue (Nonzero curve) =
  Curve.CurvatureVector.value_
    (Curve.derivativeAt tValue curve)
    (Curve.secondDerivativeAt tValue curve)

curvatureVectorRange_ ::
  (Curve.Exists dimension units space, VectorBounds.Exists dimension (Unitless ?/? units) space) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorRange_ tRange (Nonzero curve) =
  Curve.CurvatureVector.range_
    (Curve.derivativeRange tRange curve)
    (Curve.secondDerivativeRange tRange curve)
