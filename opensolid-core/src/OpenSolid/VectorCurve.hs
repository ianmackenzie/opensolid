module OpenSolid.VectorCurve
  ( IsZero (IsZero)
  , isZero
  , derivative
  , normalize
  , direction
  )
where

import OpenSolid.CoordinateSystem (CoordinateSystem (DirectionCurve, UnitlessVectorCurve, VectorCurve))
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude

data IsZero = IsZero deriving (Eq, Show)

isZero ::
  (CoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Bool
isZero = CoordinateSystem.vectorCurveIsZero

derivative ::
  CoordinateSystem dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivative = CoordinateSystem.vectorCurveDerivative

normalize ::
  (CoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  UnitlessVectorCurve dimension space
normalize = CoordinateSystem.normalizeVectorCurve

direction ::
  forall dimension units space.
  (CoordinateSystem dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsZero (DirectionCurve dimension space)
direction vectorCurve =
  if isZero vectorCurve
    then Error IsZero
    else Ok (DirectionCurve.unsafe @dimension @units @space (normalize vectorCurve))
