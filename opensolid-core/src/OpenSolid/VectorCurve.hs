module OpenSolid.VectorCurve
  ( IsZero (IsZero)
  , isZero
  , bezier
  , evaluate
  , evaluateBounds
  , derivative
  , squaredMagnitude_
  , normalize
  , direction
  , zeros
  , desingularize
  )
where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CoordinateSystem (DirectionCurve, Vector, VectorBounds, VectorCurve)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified
import OpenSolid.Desingularization qualified as Desingularization
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance

data IsZero = IsZero deriving (Eq, Show)

isZero ::
  (CoordinateSystem.Generic dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Bool
isZero = CoordinateSystem.vectorCurveIsZero

bezier ::
  CoordinateSystem.Generic dimension units space =>
  NonEmpty (Vector dimension units space) ->
  VectorCurve dimension units space
bezier = CoordinateSystem.bezierVectorCurve

evaluate ::
  CoordinateSystem.Generic dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
evaluate = CoordinateSystem.evaluateVectorCurve

evaluateBounds ::
  CoordinateSystem.Generic dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
evaluateBounds = CoordinateSystem.evaluateVectorCurveBounds

derivative ::
  CoordinateSystem.Generic dimension units space =>
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

desingularize ::
  CoordinateSystem.Generic dimension units space =>
  ( VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space ->
    VectorCurve dimension units space
  ) ->
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space ->
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space
desingularize _ Nothing curve Nothing = curve
desingularize desingularized startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = evaluate curve t0
          let firstDerivativeT0 = evaluate (derivative curve) t0
          let secondDerivativeT0 = evaluate (derivative (derivative curve)) t0
          bezier $
            Bezier.syntheticStart
              value0
              firstDerivative0
              valueT0
              firstDerivativeT0
              secondDerivativeT0
  let endCurve = case endSingularity of
        Nothing -> curve
        Just (value1, firstDerivative1) -> do
          let t1 = Desingularization.t1
          let valueT1 = evaluate curve t1
          let firstDerivativeT1 = evaluate (derivative curve) t1
          let secondDerivativeT1 = evaluate (derivative (derivative curve)) t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve
