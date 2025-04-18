module Tests.SurfaceFunction (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Error qualified as Error
import OpenSolid.Length qualified as Length
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvCoordinates, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import Test (Expectation, Test)
import Test qualified
import Tests.Curve2d qualified
import Tests.Random qualified as Random
import Tests.VectorCurve2d qualified

tests :: Tolerance Meters => List Test
tests =
  [ planeTorusIntersection
  ]

planeTorusIntersection :: Tolerance Meters => Test
planeTorusIntersection =
  Test.group "planeTorusIntersection" $
    [ firstDerivativeConsistency
    , intersectionCurveBoundsConsistency
    , intersectionCurveFirstDerivativeConsistency
    , intersectionCurveFirstDerivativeBoundsConsistency
    , intersectionCurveSecondDerivativeConsistency
    , intersectionCurveSecondDerivativeBoundsConsistency
    ]

firstDerivativeConsistency :: Test
firstDerivativeConsistency = Test.check 100 "firstDerivativeConsistency" Test.do
  uvPoint <- SurfaceParameter.random
  parameter <- Random.surfaceParameter
  firstDerivativeIsConsistent planeTorusSurface uvPoint parameter

withIntersectionCurves :: Tolerance Meters => (NonEmpty (Curve2d UvCoordinates) -> Test) -> Test
withIntersectionCurves callback =
  case SurfaceFunction.zeros planeTorusSurface of
    Failure error -> Test.abort (Error.message error)
    Success zeros -> case SurfaceFunction.Zeros.crossingCurves zeros of
      [] -> Test.abort "No intersection curves found"
      NonEmpty crossingCurves -> callback crossingCurves

intersectionCurveFirstDerivativeConsistency :: Tolerance Meters => Test
intersectionCurveFirstDerivativeConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveFirstDerivativeConsistency" Test.do
      curve <- Random.oneOf curves
      tValue <- Parameter.random
      Tests.Curve2d.firstDerivativeIsConsistentWithin 1e-6 curve tValue
        |> Test.output "tValue" tValue

intersectionCurveBoundsConsistency :: Tolerance Meters => Test
intersectionCurveBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      Tolerance.using 1e-9 (Tests.Curve2d.boundsConsistency curve)

intersectionCurveFirstDerivativeBoundsConsistency :: Tolerance Meters => Test
intersectionCurveFirstDerivativeBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      let firstDerivative = Curve2d.derivative curve
      Tolerance.using 1e-9 (Tests.VectorCurve2d.boundsConsistency firstDerivative)

intersectionCurveSecondDerivativeConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeConsistency" Test.do
      curve <- Random.oneOf curves
      let firstDerivative = Curve2d.derivative curve
      Tests.VectorCurve2d.derivativeConsistency 1e-6 firstDerivative

intersectionCurveSecondDerivativeBoundsConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      let firstDerivative = Curve2d.derivative curve
      let secondDerivative = VectorCurve2d.derivative firstDerivative
      Tolerance.using 1e-9 (Tests.VectorCurve2d.boundsConsistency secondDerivative)

planeTorusSurface :: SurfaceFunction Meters
planeTorusSurface = do
  let theta = Angle.twoPi * SurfaceFunction.u
  let phi = Angle.twoPi * SurfaceFunction.v
  let minorRadius = Length.centimeters 1.0
  let majorRadius = Length.centimeters 2.0
  let r = majorRadius + minorRadius * SurfaceFunction.cos phi
  let x = r * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.sin theta
  let z = minorRadius * SurfaceFunction.sin phi
  let alpha = Angle.asin (minorRadius / majorRadius)
  -- Other possibilities: Direction3d.xy (Angle.degrees 45), Direction3d.z
  Direction3d.zx -alpha `dot` VectorSurfaceFunction3d.xyz x y z

samplingRadius :: Float
samplingRadius = 1e-6

firstDerivativeIsConsistent :: SurfaceFunction Meters -> UvPoint -> SurfaceParameter -> Expectation
firstDerivativeIsConsistent surfaceFunction p0 parameter = do
  let partialDerivative = SurfaceFunction.derivative parameter surfaceFunction
  let (p1, p2) = samplingPoints p0 parameter
  let value1 = SurfaceFunction.evaluate surfaceFunction p1
  let value2 = SurfaceFunction.evaluate surfaceFunction p2
  let numericalDerivative = (value2 - value1) / (2.0 * samplingRadius)
  let analyticalDerivative = SurfaceFunction.evaluate partialDerivative p0
  Tolerance.using (Length.meters 1e-6) do
    Test.expect (numericalDerivative ~= analyticalDerivative)
      |> Test.output "numericalDerivative" numericalDerivative
      |> Test.output "analyticalDerivative" analyticalDerivative

samplingPoints :: UvPoint -> SurfaceParameter -> (UvPoint, UvPoint)
samplingPoints (Point2d u0 v0) parameter =
  case parameter of
    U -> (Point2d (u0 - samplingRadius) v0, Point2d (u0 + samplingRadius) v0)
    V -> (Point2d u0 (v0 - samplingRadius), Point2d u0 (v0 + samplingRadius))
