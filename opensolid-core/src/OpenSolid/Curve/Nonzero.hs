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

import OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.DirectionBounds (DirectionBounds, DirectionBoundsExists)
import OpenSolid.Interval (Interval)
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector, VectorExists)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector.Nonzero qualified as Vector.Nonzero
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
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
  (CurveExists dimension units space, VectorExists dimension units space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Nonzero (Vector dimension units space)
derivativeAt tValue curve = VectorCurve.Nonzero.valueAt tValue (derivative curve)

tangentDirectionAt ::
  (CurveExists dimension units space, DirectionExists dimension space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Direction dimension space
tangentDirectionAt tValue curve = Vector.Nonzero.direction (derivativeAt tValue curve)

tangentDirectionRange ::
  (CurveExists dimension units space, DirectionBoundsExists dimension space) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  DirectionBounds dimension space
tangentDirectionRange tRange (Nonzero curve) =
  VectorBounds.direction (Curve.derivativeRange tRange curve)

curvatureVectorAt ::
  ( CurveExists dimension units space
  , Units.Inverse units inverseUnits
  , VectorExists dimension inverseUnits space
  ) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Vector dimension inverseUnits space
curvatureVectorAt tValue curve = Vector.coerce (curvatureVectorAt_ tValue curve)

curvatureVectorRange ::
  ( CurveExists dimension units space
  , Units.Inverse units inverseUnits
  , VectorBoundsExists dimension inverseUnits space
  ) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  VectorBounds dimension inverseUnits space
curvatureVectorRange tRange curve = VectorBounds.coerce (curvatureVectorRange_ tRange curve)

curvatureVectorAt_ ::
  (CurveExists dimension units space, VectorExists dimension (Unitless ?/? units) space) =>
  Number ->
  Nonzero (Curve dimension units space) ->
  Vector dimension (Unitless ?/? units) space
curvatureVectorAt_ tValue (Nonzero curve) =
  Curve.CurvatureVector.value_
    (Curve.derivativeAt tValue curve)
    (Curve.secondDerivativeAt tValue curve)

curvatureVectorRange_ ::
  (CurveExists dimension units space, VectorBoundsExists dimension (Unitless ?/? units) space) =>
  Interval Unitless ->
  Nonzero (Curve dimension units space) ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorRange_ tRange (Nonzero curve) =
  Curve.CurvatureVector.range_
    (Curve.derivativeRange tRange curve)
    (Curve.secondDerivativeRange tRange curve)
