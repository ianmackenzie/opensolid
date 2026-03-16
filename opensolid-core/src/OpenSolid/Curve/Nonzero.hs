module OpenSolid.Curve.Nonzero
  ( curvatureVector_
  , curvatureVectorValue_
  , curvatureVectorBounds_
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

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
curvatureVectorValue_ (Nonzero curve) tValue = do
  let derivativeValue = Curve.derivativeValue curve tValue
  let secondDerivativeValue = Curve.secondDerivativeValue curve tValue
  let derivativeSquaredMagnitude_ = Vector.squaredMagnitude_ derivativeValue
  let numerator =
        secondDerivativeValue
          - derivativeValue * ((secondDerivativeValue `dot_` derivativeValue) / derivativeSquaredMagnitude_)
  Vector.unerase (Vector.erase numerator / Quantity.erase derivativeSquaredMagnitude_)

curvatureVectorBounds_ ::
  (Curve.Exists dimension units space, VectorBounds.Exists dimension (Unitless ?/? units) space) =>
  Nonzero (Curve dimension units space) ->
  Interval Unitless ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorBounds_ (Nonzero curve) tValue = do
  let derivativeBounds = Curve.derivativeBounds curve tValue
  let secondDerivativeBounds = Curve.secondDerivativeBounds curve tValue
  let derivativeSquaredMagnitude_ = VectorBounds.squaredMagnitude_ derivativeBounds
  let numerator =
        secondDerivativeBounds
          - derivativeBounds * ((secondDerivativeBounds `dot_` derivativeBounds) / derivativeSquaredMagnitude_)
  VectorBounds.unerase (VectorBounds.erase numerator / Interval.erase derivativeSquaredMagnitude_)
