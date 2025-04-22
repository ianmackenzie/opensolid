module OpenSolid.VectorCurve2d.DegenerateEndpoint
  ( DegenerateEndpoint
  , at
  , derivative
  , cutoff
  , evaluate
  , evaluateBounds
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d qualified as VectorCurve2d

data DegenerateEndpoint space
  = DegenerateEndpoint Float Float (VectorCurve2d (space @ Unitless))

at :: Tolerance units => Float -> VectorCurve2d (space @ units) -> DegenerateEndpoint space
at t0 secondDerivative = do
  let r = computeRadius (VectorCurve2d.evaluate secondDerivative t0)
  let t1 = if t0 == 0.0 then r else 1.0 - r
  let q = qCurve 0 t0 secondDerivative
  let sign = if t0 == 0.0 then Positive else Negative
  let curve = sign * q / VectorCurve2d.unsafeMagnitude q
  DegenerateEndpoint t0 t1 curve

computeRadius :: Tolerance units => Vector2d (space @ units) -> Float
computeRadius secondDerivative = Qty.sqrt (2.0 * ?tolerance / Vector2d.magnitude secondDerivative)

cutoff :: DegenerateEndpoint space -> Float
cutoff (DegenerateEndpoint _ t1 _) = t1

derivative :: DegenerateEndpoint space -> DegenerateEndpoint space
derivative (DegenerateEndpoint t0 t1 curve) =
  DegenerateEndpoint t0 t1 (VectorCurve2d.derivative curve)

qCurve :: Int -> Float -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
qCurve n t0 curveDerivative = do
  let value = VectorCurve2d.evaluate curveDerivative t0 / Float.int (n + 1)
  let compiled = CompiledFunction.concrete (Expression.constant value)
  let qCurveDerivative = qCurve (n + 1) t0 (VectorCurve2d.derivative curveDerivative)
  VectorCurve2d.new compiled qCurveDerivative

evaluate ::
  DegenerateEndpoint space ->
  VectorCurve2d (space @ Unitless) ->
  Float ->
  Vector2d (space @ Unitless)
evaluate (DegenerateEndpoint t0 t1 endpointCurve) innerCurve tValue =
  Vector2d.interpolateFrom
    (VectorCurve2d.evaluate endpointCurve t0)
    (VectorCurve2d.evaluate innerCurve t1)
    ((tValue - t0) / (t1 - t0))

evaluateBounds ::
  DegenerateEndpoint space ->
  VectorCurve2d (space @ Unitless) ->
  Bounds Unitless ->
  VectorBounds2d (space @ Unitless)
evaluateBounds (DegenerateEndpoint t0 t1 endpointCurve) innerCurve (Bounds tLow tHigh) = do
  let v0 = VectorCurve2d.evaluate endpointCurve t0
  let v1 = VectorCurve2d.evaluate innerCurve t1
  VectorBounds2d.hull2
    (Vector2d.interpolateFrom v0 v1 ((tLow - t0) / (t1 - t0)))
    (Vector2d.interpolateFrom v0 v1 ((tHigh - t0) / (t1 - t0)))

transformBy ::
  Transform2d tag (space @ units) ->
  DegenerateEndpoint space ->
  DegenerateEndpoint space
transformBy transform (DegenerateEndpoint t0 t1 endpointCurve) =
  DegenerateEndpoint t0 t1 (VectorCurve2d.transformBy transform endpointCurve)
