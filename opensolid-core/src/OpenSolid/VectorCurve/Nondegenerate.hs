module OpenSolid.VectorCurve.Nondegenerate
  ( directionAt
  , directionRange
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Direction qualified as VectorCurve.Direction

directionAt ::
  (VectorCurve.Exists dimension units space, Direction.Exists dimension space) =>
  Number ->
  Nondegenerate (VectorCurve dimension units space) ->
  Direction dimension space
directionAt tValue (Nondegenerate curve) = do
  let curveValue = VectorCurve.valueAt tValue curve
  let derivativeValue = VectorCurve.derivativeAt tValue curve
  VectorCurve.Direction.value curve tValue curveValue derivativeValue

directionRange ::
  VectorCurve.Exists dimension units space =>
  Interval Unitless ->
  Nondegenerate (VectorCurve dimension units space) ->
  DirectionBounds dimension space
directionRange tRange (Nondegenerate curve) = do
  let curveRange = VectorCurve.range tRange curve
  let derivativeRange = VectorCurve.derivativeRange tRange curve
  VectorCurve.Direction.range curve tRange curveRange derivativeRange

squaredMagnitude_ ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate (VectorCurve dimension units space) ->
  Nondegenerate (Curve1D (units ?*? units))
squaredMagnitude_ (Nondegenerate curve) = Nondegenerate (VectorCurve.squaredMagnitude_ curve)

squaredMagnitude ::
  ( VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate (VectorCurve dimension units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_
