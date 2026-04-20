module OpenSolid.Curve.Nonzero
  ( tangentDirection
  , tangentDirectionValue
  , tangentDirectionRange
  , curvatureVector_
  , curvatureVectorValue_
  , curvatureVectorRange_
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval)
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Nonzero qualified as VectorCurve.Nonzero

tangentDirection ::
  Curve.Exists dimension units space =>
  Nonzero (Curve dimension units space) ->
  DirectionCurve dimension space
tangentDirection (Nonzero curve) =
  VectorCurve.Nonzero.direction (Nonzero (Curve.derivative curve))

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

curvatureVector_ ::
  (Curve.Exists dimension units space, VectorCurve.Exists dimension (Unitless ?/? units) space) =>
  Nonzero (Curve dimension units space) ->
  VectorCurve dimension (Unitless ?/? units) space
curvatureVector_ (Nonzero curve) = do
  let derivative = Curve.derivative curve
  let secondDerivative = Curve.secondDerivative curve
  let derivativeSquaredMagnitude_ = VectorCurve.squaredMagnitude_ derivative
  let numerator =
        secondDerivative
          - derivative * ((secondDerivative `dot_` derivative) / Nonzero derivativeSquaredMagnitude_)
  VectorCurve.unerase (VectorCurve.erase numerator / Nonzero (Curve1D.erase derivativeSquaredMagnitude_))

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
