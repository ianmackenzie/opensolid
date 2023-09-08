module VectorCurve2d.DegenerateEndpoint
  ( DegenerateEndpoint
  , at
  , derivative
  , cutoff
  , evaluateAt
  , segmentBounds
  )
where

import Domain (Domain)
import Float qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified
import {-# SOURCE #-} VectorCurve2d (IsVectorCurve2d (..), VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified
import VectorCurve2d.Magnitude qualified

data DegenerateEndpoint space
  = DegenerateEndpoint Float Float (VectorCurve2d (space @ Unitless))
  deriving (Show)

at :: Tolerance units => Float -> VectorCurve2d (space @ units) -> DegenerateEndpoint space
at t0 secondDerivative =
  let r = computeRadius (VectorCurve2d.evaluateAt t0 secondDerivative)
      t1 = if t0 == 0.0 then r else 1.0 - r
      q = qCurve 0 t0 secondDerivative
      sign = if t0 == 0.0 then Positive else Negative
      curve = sign * q / VectorCurve2d.Magnitude.unsafe q
   in DegenerateEndpoint t0 t1 curve

computeRadius :: Tolerance units => Vector2d (space @ units) -> Float
computeRadius secondDerivative = Qty.sqrt (2.0 * ?tolerance / Vector2d.magnitude secondDerivative)

cutoff :: DegenerateEndpoint space -> Float
cutoff (DegenerateEndpoint _ t1 _) = t1

derivative :: DegenerateEndpoint space -> DegenerateEndpoint space
derivative (DegenerateEndpoint t0 t1 curve) =
  DegenerateEndpoint t0 t1 (VectorCurve2d.derivative curve)

data QCurve coordinateSystem
  = QCurve Int Float (VectorCurve2d coordinateSystem) (Vector2d coordinateSystem)
  deriving (Show)

instance IsVectorCurve2d (QCurve (space @ units)) (space @ units) where
  evaluateAtImpl _ (QCurve _ _ _ value) = value
  segmentBoundsImpl _ (QCurve _ _ _ value) = VectorBox2d.constant value
  derivativeImpl (QCurve n t0 curveDerivative _) =
    qCurve (n + 1) t0 (VectorCurve2d.derivative curveDerivative)

qCurve :: Int -> Float -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
qCurve n t0 curveDerivative =
  VectorCurve2d.wrap $
    QCurve n t0 curveDerivative $
      (VectorCurve2d.evaluateAt t0 curveDerivative / (Float.fromInt n + 1.0))

evaluateAt :: Float -> DegenerateEndpoint space -> VectorCurve2d (space @ Unitless) -> Vector2d (space @ Unitless)
evaluateAt t (DegenerateEndpoint t0 t1 endpointCurve) innerCurve =
  Vector2d.interpolateFrom
    (VectorCurve2d.evaluateAt t0 endpointCurve)
    (VectorCurve2d.evaluateAt t1 innerCurve)
    ((t - t0) / (t1 - t0))

segmentBounds :: Domain -> DegenerateEndpoint space -> VectorCurve2d (space @ Unitless) -> VectorBox2d (space @ Unitless)
segmentBounds (Range tLow tHigh) (DegenerateEndpoint t0 t1 endpointCurve) innerCurve =
  let v0 = VectorCurve2d.evaluateAt t0 endpointCurve
      v1 = VectorCurve2d.evaluateAt t1 innerCurve
   in VectorBox2d.hull2
        (Vector2d.interpolateFrom v0 v1 ((tLow - t0) / (t1 - t0)))
        (Vector2d.interpolateFrom v0 v1 ((tHigh - t0) / (t1 - t0)))
