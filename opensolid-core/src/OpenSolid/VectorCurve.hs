module OpenSolid.VectorCurve
  ( IsZero (IsZero)
  , isZero
  , derivative
  , squaredMagnitude_
  , normalize
  , direction
  , zeros
  )
where

import OpenSolid.CoordinateSystem (DirectionCurve, VectorCurve)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance

data IsZero = IsZero deriving (Eq, Show)

isZero ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Bool
isZero = CoordinateSystem.vectorCurveIsZero

derivative ::
  CoordinateSystem.Vectorial dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivative = CoordinateSystem.vectorCurveDerivative

squaredMagnitude_ ::
  CoordinateSystem.Vectorial dimension units space =>
  VectorCurve dimension units space ->
  Curve1D (units ?*? units)
squaredMagnitude_ = CoordinateSystem.vectorCurveSquaredMagnitude_

normalize ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  VectorCurve dimension Unitless space
normalize = CoordinateSystem.normalizeVectorCurve

direction ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (DirectionCurve dimension space)
direction vectorCurve =
  if isZero vectorCurve
    then Error IsZero
    else Ok (DirectionCurve.unsafe (normalize vectorCurve))

zeros ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (List Number)
zeros vectorCurve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.zeros (squaredMagnitude_ vectorCurve)) of
    Ok zeros1D -> Ok (List.map (.location) zeros1D)
    Error Curve1D.IsZero -> Error IsZero
