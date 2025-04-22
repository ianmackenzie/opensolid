module OpenSolid.VectorCurve3d.DegenerateEndpoint
  ( DegenerateEndpoint
  , at
  , derivative
  , cutoff
  , evaluate
  , evaluateBounds
  , transformBy
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d

data DegenerateEndpoint space
  = DegenerateEndpoint Float Float (VectorCurve3d (space @ Unitless))

at :: Tolerance units => Float -> VectorCurve3d (space @ units) -> DegenerateEndpoint space
at t0 secondDerivative = do
  let r = computeRadius (VectorCurve3d.evaluate secondDerivative t0)
  let t1 = if t0 == 0.0 then r else 1.0 - r
  let q = qCurve 0 t0 secondDerivative
  let sign = if t0 == 0.0 then Positive else Negative
  let curve = sign * q / VectorCurve3d.unsafeMagnitude q
  DegenerateEndpoint t0 t1 curve

computeRadius :: Tolerance units => Vector3d (space @ units) -> Float
computeRadius secondDerivative = Qty.sqrt (2.0 * ?tolerance / Vector3d.magnitude secondDerivative)

cutoff :: DegenerateEndpoint space -> Float
cutoff (DegenerateEndpoint _ t1 _) = t1

derivative :: DegenerateEndpoint space -> DegenerateEndpoint space
derivative (DegenerateEndpoint t0 t1 curve) =
  DegenerateEndpoint t0 t1 (VectorCurve3d.derivative curve)

qCurve :: Int -> Float -> VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
qCurve n t0 curveDerivative = do
  let value = VectorCurve3d.evaluate curveDerivative t0 / Float.int (n + 1)
  let compiled = CompiledFunction.concrete (Expression.constant value)
  let qCurveDerivative = qCurve (n + 1) t0 (VectorCurve3d.derivative curveDerivative)
  VectorCurve3d.new compiled qCurveDerivative

evaluate ::
  DegenerateEndpoint space ->
  VectorCurve3d (space @ Unitless) ->
  Float ->
  Vector3d (space @ Unitless)
evaluate (DegenerateEndpoint t0 t1 endpointCurve) innerCurve tValue =
  Vector3d.interpolateFrom
    (VectorCurve3d.evaluate endpointCurve t0)
    (VectorCurve3d.evaluate innerCurve t1)
    ((tValue - t0) / (t1 - t0))

evaluateBounds ::
  DegenerateEndpoint space ->
  VectorCurve3d (space @ Unitless) ->
  Bounds Unitless ->
  VectorBounds3d (space @ Unitless)
evaluateBounds (DegenerateEndpoint t0 t1 endpointCurve) innerCurve (Bounds tLow tHigh) = do
  let v0 = VectorCurve3d.evaluate endpointCurve t0
  let v1 = VectorCurve3d.evaluate innerCurve t1
  VectorBounds3d.hull2
    (Vector3d.interpolateFrom v0 v1 ((tLow - t0) / (t1 - t0)))
    (Vector3d.interpolateFrom v0 v1 ((tHigh - t0) / (t1 - t0)))

transformBy ::
  Transform3d tag (space @ units) ->
  DegenerateEndpoint space ->
  DegenerateEndpoint space
transformBy transform (DegenerateEndpoint t0 t1 endpointCurve) =
  DegenerateEndpoint t0 t1 (VectorCurve3d.transformBy transform endpointCurve)
