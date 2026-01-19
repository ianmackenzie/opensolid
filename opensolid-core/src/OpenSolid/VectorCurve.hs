module OpenSolid.VectorCurve
  ( IsZero (IsZero)
  , isZero
  , derivative
  , squaredMagnitude_
  , normalize
  , direction
  )
where

import OpenSolid.CoordinateSystem (VectorCoordinateSystem (DirectionCurve, VectorCurve))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude

data IsZero = IsZero deriving (Eq, Show)

isZero ::
  (VectorCoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Bool
isZero = CoordinateSystem.vectorCurveIsZero

derivative ::
  VectorCoordinateSystem dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivative = CoordinateSystem.vectorCurveDerivative

squaredMagnitude_ ::
  VectorCoordinateSystem dimension units space =>
  VectorCurve dimension units space ->
  Curve1D (units ?*? units)
squaredMagnitude_ = CoordinateSystem.vectorCurveSquaredMagnitude_

normalize ::
  (VectorCoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  VectorCurve dimension Unitless space
normalize = CoordinateSystem.normalizeVectorCurve

direction ::
  forall dimension units space.
  (VectorCoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (DirectionCurve dimension space)
direction vectorCurve =
  if isZero vectorCurve
    then Error IsZero
    else Ok (DirectionCurve.unsafe @dimension @units @space (normalize vectorCurve))
