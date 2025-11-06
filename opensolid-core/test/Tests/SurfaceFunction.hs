module Tests.SurfaceFunction (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve2d (Curve2d)
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
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.World3d qualified as World3d
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
  uvPoint <- UvPoint.random
  parameter <- Random.surfaceParameter
  firstDerivativeIsConsistent planeTorusSurface uvPoint parameter

withIntersectionCurves :: Tolerance Meters => (NonEmpty (Curve2d UvCoordinates) -> Test) -> Test
withIntersectionCurves callback =
  case SurfaceFunction.zeros planeTorusSurface of
    Failure error -> Test.abort (Error.message error)
    Success zeros -> case zeros.crossingCurves of
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
      Tolerance.using 1e-9 (Tests.VectorCurve2d.boundsConsistency curve.derivative)

intersectionCurveSecondDerivativeConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeConsistency" Test.do
      curve <- Random.oneOf curves
      Tests.VectorCurve2d.derivativeConsistency 1e-6 curve.derivative

intersectionCurveSecondDerivativeBoundsConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      Tolerance.using 1e-9 (Tests.VectorCurve2d.boundsConsistency curve.derivative.derivative)

planeTorusSurface :: SurfaceFunction Meters
planeTorusSurface = do
  let theta = Angle.twoPi * SurfaceFunction.u
  let phi = Angle.twoPi * SurfaceFunction.v
  let minorRadius = Length.centimeters 1.0
  let majorRadius = Length.centimeters 2.0
  let r = majorRadius + minorRadius * SurfaceFunction.cos phi
  let alpha = Angle.asin (minorRadius ./. majorRadius)
  let normalDirection = Direction3d.polar World3d.frontPlane (alpha + Angle.halfPi)
  let surfaceFunction =
        r * SurfaceFunction.cos theta * World3d.rightwardDirection
          + r * SurfaceFunction.sin theta * World3d.forwardDirection
          + minorRadius * SurfaceFunction.sin phi * World3d.upwardDirection
  normalDirection `dot` surfaceFunction

samplingRadius :: Number
samplingRadius = 1e-6

firstDerivativeIsConsistent :: SurfaceFunction Meters -> UvPoint -> SurfaceParameter -> Expectation
firstDerivativeIsConsistent surfaceFunction p0 parameter = do
  let partialDerivative = SurfaceFunction.derivative parameter surfaceFunction
  let (p1, p2) = samplingPoints p0 parameter
  let value1 = SurfaceFunction.evaluate surfaceFunction p1
  let value2 = SurfaceFunction.evaluate surfaceFunction p2
  let numericalDerivative = (value2 - value1) ./. (2.0 *. samplingRadius)
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
