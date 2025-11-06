module Tests.VectorCurve2d (boundsConsistency, derivativeConsistency) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude hiding ((+), (-))
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import Test (Expectation)
import Test qualified
import Prelude ((+), (-))

derivativeConsistency ::
  Show (Quantity units) =>
  Quantity units ->
  VectorCurve2d (space @ units) ->
  Expectation
derivativeConsistency givenTolerance curve = Test.do
  tValue <- Parameter.random
  let dt = 1e-6
  let v1 = VectorCurve2d.evaluate curve (tValue - dt)
  let v2 = VectorCurve2d.evaluate curve (tValue + dt)
  let numericalFirstDerivative = (v2 .-. v1) ./ (2.0 *. dt)
  let analyticFirstDerivative = VectorCurve2d.evaluate curve.derivative tValue
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      |> Test.output "numericalFirstDerivative" numericalFirstDerivative
      |> Test.output "analyticFirstDerivative" analyticFirstDerivative

boundsConsistency ::
  (Tolerance units, Show (Quantity units)) =>
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
