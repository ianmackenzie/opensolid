module Tests.SurfaceFunction (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Length qualified as Length
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.World3D qualified as World3D
import Test (Expectation, Test)
import Test qualified
import Tests.Curve2D qualified
import Tests.Random qualified as Random
import Tests.VectorCurve2D qualified

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

withIntersectionCurves :: Tolerance Meters => (NonEmpty (Curve2D Unitless UvSpace) -> Test) -> Test
withIntersectionCurves callback =
  case SurfaceFunction.zeros planeTorusSurface of
    Error error -> Test.abort (Text.show error)
    Ok zeros -> case zeros.crossingCurves of
      [] -> Test.abort "No intersection curves found"
      NonEmpty crossingCurves -> callback crossingCurves

intersectionCurveFirstDerivativeConsistency :: Tolerance Meters => Test
intersectionCurveFirstDerivativeConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveFirstDerivativeConsistency" Test.do
      curve <- Random.oneOf curves
      tValue <- Parameter.random
      Tests.Curve2D.firstDerivativeIsConsistentWithin 1e-6 curve tValue
        & Test.output "tValue" tValue

intersectionCurveBoundsConsistency :: Tolerance Meters => Test
intersectionCurveBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      Tolerance.using 1e-9 (Tests.Curve2D.boundsConsistency curve)

intersectionCurveFirstDerivativeBoundsConsistency :: Tolerance Meters => Test
intersectionCurveFirstDerivativeBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      Tolerance.using 1e-9 (Tests.VectorCurve2D.boundsConsistency (Curve2D.derivative curve))

intersectionCurveSecondDerivativeConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeConsistency" Test.do
      curve <- Random.oneOf curves
      Tests.VectorCurve2D.derivativeConsistency 1e-6 (Curve2D.derivative curve)

intersectionCurveSecondDerivativeBoundsConsistency :: Tolerance Meters => Test
intersectionCurveSecondDerivativeBoundsConsistency =
  withIntersectionCurves \curves ->
    Test.check 100 "intersectionCurveSecondDerivativeBoundsConsistency" Test.do
      curve <- Random.oneOf curves
      let firstDerivative = Curve2D.derivative curve
      let secondDerivative = VectorCurve2D.derivative firstDerivative
      Tolerance.using 1e-9 (Tests.VectorCurve2D.boundsConsistency secondDerivative)

planeTorusSurface :: SurfaceFunction Meters
planeTorusSurface = do
  let theta = Angle.twoPi .*. SurfaceFunction.u
  let phi = Angle.twoPi .*. SurfaceFunction.v
  let minorRadius = Length.centimeters 1
  let majorRadius = Length.centimeters 2
  let r = majorRadius .+. minorRadius .*. SurfaceFunction.cos phi
  let alpha = Angle.asin (minorRadius ./. majorRadius)
  let normalDirection = Direction3D.polar World3D.frontPlane (alpha .+. Angle.halfPi)
  let surfaceFunction =
        r .*. SurfaceFunction.cos theta .*. World3D.rightwardDirection
          .+. r .*. SurfaceFunction.sin theta .*. World3D.forwardDirection
          .+. minorRadius .*. SurfaceFunction.sin phi .*. World3D.upwardDirection
  normalDirection `dot` surfaceFunction

samplingRadius :: Number
samplingRadius = 1e-6

firstDerivativeIsConsistent :: SurfaceFunction Meters -> UvPoint -> SurfaceParameter -> Expectation
firstDerivativeIsConsistent surfaceFunction p0 parameter = do
  let partialDerivative = SurfaceFunction.derivative parameter surfaceFunction
  let (p1, p2) = samplingPoints p0 parameter
  let value1 = SurfaceFunction.evaluate surfaceFunction p1
  let value2 = SurfaceFunction.evaluate surfaceFunction p2
  let numericalDerivative = (value2 .-. value1) ./. (2 *. samplingRadius)
  let analyticalDerivative = SurfaceFunction.evaluate partialDerivative p0
  Tolerance.using Length.micrometer do
    Test.expect (numericalDerivative ~= analyticalDerivative)
      & Test.output "numericalDerivative" numericalDerivative
      & Test.output "analyticalDerivative" analyticalDerivative

samplingPoints :: UvPoint -> SurfaceParameter -> (UvPoint, UvPoint)
samplingPoints (UvPoint u0 v0) parameter =
  case parameter of
    U -> (UvPoint (u0 .-. samplingRadius) v0, UvPoint (u0 .+. samplingRadius) v0)
    V -> (UvPoint u0 (v0 .-. samplingRadius), UvPoint u0 (v0 .+. samplingRadius))
