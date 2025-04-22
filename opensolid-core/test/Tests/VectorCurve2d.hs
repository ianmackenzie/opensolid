module Tests.VectorCurve2d (boundsConsistency, derivativeConsistency) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import Test (Expectation)
import Test qualified

derivativeConsistency ::
  Show (Qty units) =>
  Qty units ->
  VectorCurve2d (space @ units) ->
  Expectation
derivativeConsistency givenTolerance curve = Test.do
  tValue <- Parameter.random
  let firstDerivative = VectorCurve2d.derivative curve
  let dt = 1e-6
  let p1 = VectorCurve2d.evaluate curve (tValue - dt)
  let p2 = VectorCurve2d.evaluate curve (tValue + dt)
  let numericalFirstDerivative = (p2 - p1) / (2.0 * dt)
  let analyticFirstDerivative = VectorCurve2d.evaluate firstDerivative tValue
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      |> Test.output "numericalFirstDerivative" numericalFirstDerivative
      |> Test.output "analyticFirstDerivative" analyticFirstDerivative

boundsConsistency ::
  (Tolerance units, Show (Qty units)) =>
  VectorCurve2d (space @ units) ->
  Expectation
boundsConsistency vectorCurve = Test.do
  tBounds <- Bounds.random Parameter.random
  tValue <- Random.map (Bounds.interpolate tBounds) Parameter.random
  let vectorCurveValue = VectorCurve2d.evaluate vectorCurve tValue
  let vectorCurveBounds = VectorCurve2d.evaluateBounds vectorCurve tBounds
  Test.expect (vectorCurveValue ^ vectorCurveBounds)
    |> Test.output "tValue" tValue
    |> Test.output "tBounds" tBounds
    |> Test.output "vectorCurveValue" vectorCurveValue
    |> Test.output "vectorCurveBounds" vectorCurveBounds
