module Tests.VectorCurve2D (boundsConsistency, derivativeConsistency) where

import OpenSolid.Interval qualified as Interval
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import Test (Expectation)
import Test qualified

derivativeConsistency ::
  Show (Quantity units) =>
  Quantity units ->
  VectorCurve2D units space ->
  Expectation
derivativeConsistency givenTolerance curve = do
  tValue <- Test.generate Parameter.random
  let dt :: Number = 1e-6
  let v1 = VectorCurve2D.value curve (tValue - dt)
  let v2 = VectorCurve2D.value curve (tValue + dt)
  let numericalFirstDerivative = (v2 - v1) / (2.0 * dt)
  let analyticFirstDerivative = VectorCurve2D.value (VectorCurve2D.derivative curve) tValue
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      & Test.output "numericalFirstDerivative" numericalFirstDerivative
      & Test.output "analyticFirstDerivative" analyticFirstDerivative

boundsConsistency ::
  (Tolerance units, Show (Quantity units)) =>
  VectorCurve2D units space ->
  Expectation
boundsConsistency vectorCurve = do
  tBounds <- Test.generate (Interval.random Parameter.random)
  tValue <- Test.generate (Random.map (Interval.interpolate tBounds) Parameter.random)
  let vectorCurveValue = VectorCurve2D.value vectorCurve tValue
  let vectorCurveBounds = VectorCurve2D.bounds vectorCurve tBounds
  Test.expect (vectorCurveValue `intersects` vectorCurveBounds)
    & Test.output "tValue" tValue
    & Test.output "tBounds" tBounds
    & Test.output "vectorCurveValue" vectorCurveValue
    & Test.output "vectorCurveBounds" vectorCurveBounds
