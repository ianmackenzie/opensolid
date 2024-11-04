module VectorCurve3d.DegenerateEndpoint
  ( DegenerateEndpoint
  , at
  , derivative
  , cutoff
  , evaluate
  , evaluateBounds
  , transformBy
  )
where

import Float qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Transform3d (Transform3d)
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} VectorCurve3d qualified

data DegenerateEndpoint space
  = DegenerateEndpoint Float Float (VectorCurve3d (space @ Unitless))
  deriving (Show)

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

data QCurve coordinateSystem where
  QCurve ::
    Int ->
    Float ->
    VectorCurve3d (space @ units) ->
    Vector3d (space @ units) ->
    QCurve (space @ units)

deriving instance Show (QCurve (space @ units))

instance VectorCurve3d.Interface (QCurve (space @ units)) (space @ units) where
  evaluateImpl (QCurve _ _ _ value) _ = value

  evaluateBoundsImpl (QCurve _ _ _ value) _ = VectorBounds3d.constant value

  derivativeImpl (QCurve n t0 curveDerivative _) =
    qCurve (n + 1) t0 (VectorCurve3d.derivative curveDerivative)

  transformByImpl transform (QCurve n t0 curveDerivative value) = do
    let transformedCurveDerivative = VectorCurve3d.transformBy transform curveDerivative
    let transformedValue = Vector3d.transformBy transform value
    VectorCurve3d.new (QCurve n t0 transformedCurveDerivative transformedValue)

qCurve :: Int -> Float -> VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
qCurve n t0 curveDerivative =
  VectorCurve3d.new $
    QCurve n t0 curveDerivative $
      VectorCurve3d.evaluate curveDerivative t0 / Float.int (n + 1)

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
  Range Unitless ->
  VectorBounds3d (space @ Unitless)
evaluateBounds (DegenerateEndpoint t0 t1 endpointCurve) innerCurve (Range tLow tHigh) = do
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
