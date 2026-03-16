module OpenSolid.Curve.CurvatureVector (value_, bounds_) where

import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds

value_ ::
  (Vector.Exists dimension units space, Vector.Exists dimension (Unitless ?/? units) space) =>
  Vector dimension units space ->
  Vector dimension units space ->
  Vector dimension (Unitless ?/? units) space
value_ derivativeValue secondDerivativeValue = do
  let derivativeSquaredMagnitude_ = Vector.squaredMagnitude_ derivativeValue
  let numerator =
        secondDerivativeValue
          - derivativeValue * ((secondDerivativeValue `dot_` derivativeValue) / derivativeSquaredMagnitude_)
  Vector.unerase (Vector.erase numerator / Quantity.erase derivativeSquaredMagnitude_)

bounds_ ::
  ( VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  ) =>
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  VectorBounds dimension (Unitless ?/? units) space
bounds_ derivativeBounds secondDerivativeBounds = do
  let derivativeSquaredMagnitude_ = VectorBounds.squaredMagnitude_ derivativeBounds
  let numerator =
        secondDerivativeBounds
          - derivativeBounds * ((secondDerivativeBounds `dot_` derivativeBounds) / derivativeSquaredMagnitude_)
  VectorBounds.unerase (VectorBounds.erase numerator / Interval.erase derivativeSquaredMagnitude_)
