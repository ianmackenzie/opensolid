module Tests.Curve2d
  ( tests
  , firstDerivativeIsConsistent
  , secondDerivativeIsConsistent
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Error qualified as Error
import OpenSolid.Float qualified as Float
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Range (Range (Range))
import OpenSolid.Sign qualified as Sign
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import Test (Expectation, Test)
import Test qualified
import Tests.Random qualified as Random

curveGenerators :: Tolerance Meters => List (Text, Generator (Curve2d (space @ Meters)))
curveGenerators =
  [ ("Line2d", Random.line2d)
  , ("Arc2d", Random.arc2d)
  , ("QuadraticSpline2d", Random.quadraticSpline2d)
  , ("CubicSpline2d", Random.cubicSpline2d)
  ]

tests :: Tolerance Meters => List Test
tests =
  [ findPoint
  , curveOverlap1
  , curveOverlap2
  , crossingIntersection
  , tangentIntersection
  , solving
  , degenerateStartPointTangent
  , degenerateEndPointTangent
  , tangentDerivativeIsPerpendicularToTangent
  , degenerateStartPointTangentDerivative
  , degenerateEndPointTangentDerivative
  , derivativeConsistency
  , reversalConsistency
  , degeneracyRemoval
  , arcConstruction
  , arcDeformation
  ]

findPoint :: Tolerance Meters => Test
findPoint = Test.verify "findPoint" Test.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = Curve2d.quadraticBezier p1 p2 p3
  startParameterValues <- Curve2d.findPoint Point2d.origin testSpline
  endParameterValues <- Curve2d.findPoint (Point2d.meters 2.0 0.0) testSpline
  midParameterValues <- Curve2d.findPoint (Point2d.meters 1.0 1.0) testSpline
  offCurveParameterValues <- Curve2d.findPoint (Point2d.meters 1.0 1.1) testSpline
  Tolerance.using 1e-12 do
    Test.all
      [ Test.expect (startParameterValues ~= [0.0])
      , Test.expect (endParameterValues ~= [1.0])
      , Test.expect (midParameterValues ~= [0.5])
      , Test.expect (offCurveParameterValues == [])
      ]

overlappingSegments ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Text (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Success (Just (Curve2d.OverlappingSegments segments)) -> Success segments
    Success (Just (Curve2d.IntersectionPoints _)) ->
      Failure "Should have found some overlapping segments, got intersection points instead"
    Success Nothing -> Failure "Should have found some overlapping segments"
    Failure error -> Failure (Error.message error)

equalUBounds :: Range Unitless -> Range Unitless -> Bool
equalUBounds (Range actualLow actualHigh) (Range expectedLow expectedHigh) =
  Tolerance.using 1e-12 (actualLow ~= expectedLow && actualHigh ~= expectedHigh)

equalOverlapSegments :: OverlappingSegment -> OverlappingSegment -> Bool
equalOverlapSegments segment1 segment2 = do
  let (OverlappingSegment actual1 actual2 actualSign) = segment1
  let (OverlappingSegment expected1 expected2 expectedSign) = segment2
  equalUBounds actual1 expected1 && equalUBounds actual2 expected2 && actualSign == expectedSign

equalOverlapSegmentLists :: NonEmpty OverlappingSegment -> NonEmpty OverlappingSegment -> Bool
equalOverlapSegmentLists actualSegments expectedSegments =
  NonEmpty.length actualSegments == NonEmpty.length expectedSegments
    && NonEmpty.allTrue (NonEmpty.map2 equalOverlapSegments actualSegments expectedSegments)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "curveOverlap1" Test.do
  let arc1 = Curve2d.arc (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) Angle.halfTurn
  let arc2 = Curve2d.arc (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) Angle.halfTurn
  actualSegments <- overlappingSegments arc1 arc2
  let expectedSegments = NonEmpty.one (OverlappingSegment (Range 0.0 0.5) (Range 0.5 1.0) Positive)
  Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "curveOverlap2" Test.do
  let arc1 = Curve2d.polarArc Point2d.origin Length.meter Angle.zero -Angle.pi
  let arc2 = Curve2d.polarArc Point2d.origin Length.meter (Angle.degrees -45.0) (Angle.degrees 225.0)
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.two
          (OverlappingSegment (Range 0.0 (1 / 4)) (Range 0.0 (1 / 6)) Negative)
          (OverlappingSegment (Range (3 / 4) 1.0) (Range (5 / 6) 1.0) Negative)
  Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "crossingIntersection" Test.do
  let arc1 = Curve2d.arc Point2d.origin (Point2d.meters 0.0 1.0) Angle.halfTurn
  let arc2 = Curve2d.arc Point2d.origin (Point2d.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.two
          (IntersectionPoint.crossing 0.0 0.0 Positive)
          (IntersectionPoint.crossing 0.5 0.5 Negative)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 (Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints))
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "tangentIntersection" Test.do
  let arc1 = Curve2d.polarArc Point2d.origin Length.meter Angle.zero Angle.pi
  let arc2 = Curve2d.polarArc (Point2d.meters 0.0 1.5) (Length.meters 0.5) -Angle.pi Angle.zero
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints = NonEmpty.one (IntersectionPoint.tangent 0.5 0.5 Positive)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 do
        Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints)
          |> Test.output "expectedIntersectionPoints" expectedIntersectionPoints
          |> Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

solving :: Tolerance Meters => Test
solving = Test.verify "solving" Test.do
  let arc = Curve2d.arc (Point2d.meters 0.0 1.0) (Point2d.meters 1.0 0.0) Angle.quarterTurn
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  let desiredDistance = Length.meters 0.5
  zeros <-
    Tolerance.using (Tolerance.ofSquared desiredDistance) do
      Curve.zeros (squaredDistanceFromOrigin - Qty.squared desiredDistance)
  let distances =
        zeros
          |> List.map Curve.Zero.location
          |> List.map (Curve2d.evaluate arc)
          |> List.map (Point2d.distanceFrom Point2d.origin)
  Test.expect (distances ~= [desiredDistance, desiredDistance])

degenerateStartPointTangent :: Tolerance Meters => Test
degenerateStartPointTangent = Test.check 100 "degenerateStartPointTangent" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = Curve2d.cubicBezier p0 p0 p1 p2
  let decreasingTValues = [2.0 ** -n | n <- [8 .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let startTangent = DirectionCurve2d.startValue tangentDirection
  let otherTangents = List.map (DirectionCurve2d.evaluate tangentDirection) decreasingTValues
  let angleDifference otherTangent = Qty.abs (Direction2d.angleFrom startTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

degenerateEndPointTangent :: Tolerance Meters => Test
degenerateEndPointTangent = Test.check 100 "degenerateEndPointTangent" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = Curve2d.cubicBezier p0 p1 p2 p2
  let increasingTValues = [1.0 - 2.0 ** -n | n <- [8 .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let endTangent = DirectionCurve2d.endValue tangentDirection
  let otherTangents = List.map (DirectionCurve2d.evaluate tangentDirection) increasingTValues
  let angleDifference otherTangent = Qty.abs (Direction2d.angleFrom endTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

tangentDerivativeIsPerpendicularToTangent :: Tolerance Meters => Test
tangentDerivativeIsPerpendicularToTangent =
  Test.check 100 "tangentDerivativeIsPerpendicularToTangent" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    p3 <- Random.point2d
    let curve = Curve2d.cubicBezier p0 p1 p2 p3
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    tValue <- Parameter.random
    let tangent = DirectionCurve2d.evaluate tangentDirection tValue
    let derivative = VectorCurve2d.evaluate tangentDerivative tValue
    Test.expect (Tolerance.using 1e-12 (derivative `dot` tangent ~= 0.0))
      |> Test.output "tValue" tValue
      |> Test.output "tangent" tangent
      |> Test.output "derivative" derivative
      |> Test.output "dot product" (derivative `dot` tangent)

degenerateStartPointTangentDerivative :: Tolerance Meters => Test
degenerateStartPointTangentDerivative =
  Test.check 100 "degenerateStartPointTangentDerivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = Curve2d.cubicBezier p0 p0 p1 p2
    let decreasingTValues = [2.0 ** -n | n <- [8 .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let startTangentDerivative = VectorCurve2d.startValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2d.evaluate tangentDerivative) decreasingTValues
    let differences =
          otherTangentDerivatives
            |> List.map (- startTangentDerivative)
            |> List.map Vector2d.magnitude
    Test.expect (List.isDescending differences)
      |> Test.output "differences" differences
      |> Test.output "startTangentDerivative" startTangentDerivative

degenerateEndPointTangentDerivative :: Tolerance Meters => Test
degenerateEndPointTangentDerivative =
  Test.check 100 "degenerateEndPointTangentDerivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = Curve2d.cubicBezier p0 p1 p2 p2
    let increasingTValues = [1.0 - 2.0 ** -n | n <- [8 .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let endTangentDerivative = VectorCurve2d.endValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2d.evaluate tangentDerivative) increasingTValues
    let differences =
          otherTangentDerivatives
            |> List.map (- endTangentDerivative)
            |> List.map Vector2d.magnitude
    Test.expect (List.isDescending differences)
      |> Test.output "curve" curve
      |> Test.output "tangentDirection" tangentDirection
      |> Test.output "tangentDerivative" tangentDerivative
      |> Test.output "differences" differences
      |> Test.output "endTangentDerivative" endTangentDerivative

firstDerivativeIsConsistent :: Curve2d (space @ Meters) -> Float -> Expectation
firstDerivativeIsConsistent curve tValue = do
  let firstDerivative = Curve2d.derivative curve
  let dt = 1e-6
  let p1 = Curve2d.evaluate curve (tValue - dt)
  let p2 = Curve2d.evaluate curve (tValue + dt)
  let numericalFirstDerivative = (p2 - p1) / (2.0 * dt)
  let analyticFirstDerivative = VectorCurve2d.evaluate firstDerivative tValue
  Tolerance.using (Length.meters 1e-6) do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      |> Test.output "numericalFirstDerivative" numericalFirstDerivative
      |> Test.output "analyticFirstDerivative" analyticFirstDerivative

firstDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
firstDerivativeConsistency curveGenerator = Test.check 100 "firstDerivativeConsistency" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  firstDerivativeIsConsistent curve t

secondDerivativeIsConsistent :: Curve2d (space @ Meters) -> Float -> Expectation
secondDerivativeIsConsistent curve tValue = do
  let firstDerivative = Curve2d.derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  let dt = 1e-6
  let v1 = VectorCurve2d.evaluate firstDerivative (tValue - dt)
  let v2 = VectorCurve2d.evaluate firstDerivative (tValue + dt)
  let numericalSecondDerivative = (v2 - v1) / (2.0 * dt)
  let analyticSecondDerivative = VectorCurve2d.evaluate secondDerivative tValue
  Tolerance.using (Length.meters 1e-6) do
    Test.expect (numericalSecondDerivative ~= analyticSecondDerivative)
      |> Test.output "numericalSecondDerivative" numericalSecondDerivative
      |> Test.output "analyticSecondDerivative" analyticSecondDerivative

secondDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
secondDerivativeConsistency curveGenerator = Test.check 100 "secondDerivativeConsistency" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  secondDerivativeIsConsistent curve t

derivativeConsistency :: Tolerance Meters => Test
derivativeConsistency =
  Test.group "derivativeConsistency" $
    List.forEach curveGenerators $
      \(label, generator) ->
        Test.group label $
          [ firstDerivativeConsistency generator
          , secondDerivativeConsistency generator
          ]

reversalConsistency :: Tolerance Meters => Test
reversalConsistency =
  Test.group "reversalConsistency" $
    List.forEach curveGenerators $
      \(label, generator) ->
        Test.check 100 label Test.do
          curve <- generator
          let reversedCurve = Curve2d.reverse curve
          t <- Parameter.random
          Test.expect (Curve2d.evaluate curve t ~= Curve2d.evaluate reversedCurve (1.0 - t))

degeneracyRemoval :: Tolerance Meters => Test
degeneracyRemoval = Test.check 100 "degeneracyRemoval" Test.do
  arcCenter <- Random.point2d
  arcRadius <- Qty.random (Length.meters 0.1) (Length.meters 10.0)
  arcStartAngle <- Qty.random -Angle.pi Angle.pi
  arcSweptAngle <- Random.map (Angle.degree *) Sign.random
  let arcEndAngle = arcStartAngle + arcSweptAngle
  t <- Parameter.random

  let arc = Curve2d.polarArc arcCenter arcRadius arcStartAngle arcEndAngle
  let arcFirstDerivative = Curve2d.derivative arc
  let arcSecondDerivative = VectorCurve2d.derivative arcFirstDerivative
  let arcThirdDerivative = VectorCurve2d.derivative arcSecondDerivative

  let startPoint = Curve2d.startPoint arc
  let startFirstDerivative = VectorCurve2d.startValue arcFirstDerivative
  let startSecondDerivative = VectorCurve2d.startValue arcSecondDerivative
  let startDerivatives = [startFirstDerivative, startSecondDerivative]

  let interpolatedCurve = Curve2d.removeStartDegeneracy 2 startPoint startDerivatives arc
  let interpolatedFirstDerivative = Curve2d.derivative interpolatedCurve
  let interpolatedSecondDerivative = VectorCurve2d.derivative interpolatedFirstDerivative
  let interpolatedThirdDerivative = VectorCurve2d.derivative interpolatedSecondDerivative

  let arcPoint = Curve2d.evaluate arc t
  let interpolatedPoint = Curve2d.evaluate interpolatedCurve t
  let expectEqualPoints =
        Tolerance.using (Length.meters 1e-12) do
          Test.expect (arcPoint ~= interpolatedPoint)
            |> Test.output "point distance" (Point2d.distanceFrom arcPoint interpolatedPoint)
  let expectEqualDerivatives n arcDerivative interpolatedDerivative = do
        let arcEndValue = VectorCurve2d.endValue arcDerivative
        let interpolatedEndValue = VectorCurve2d.endValue interpolatedDerivative
        Test.expect (arcEndValue ~= interpolatedEndValue)
          |> Test.output "Derivative order" n
          |> Test.output "Derivative at end of arc" arcEndValue
          |> Test.output "Derivative at start of interpolated curve" interpolatedEndValue
          |> Test.output "Derivative difference magnitude" (Vector2d.magnitude (interpolatedEndValue - arcEndValue))
  Test.all
    [ expectEqualPoints
    , Tests.Curve2d.firstDerivativeIsConsistent interpolatedCurve t
    , Tests.Curve2d.secondDerivativeIsConsistent interpolatedCurve t
    , expectEqualDerivatives 1 arcFirstDerivative interpolatedFirstDerivative
    , expectEqualDerivatives 2 arcSecondDerivative interpolatedSecondDerivative
    , expectEqualDerivatives 3 arcThirdDerivative interpolatedThirdDerivative
    ]

arcConstruction :: Tolerance Meters => Test
arcConstruction = do
  let testArcMidpoint numDegrees (expectedX, expectedY) = do
        let label = Text.int numDegrees + " degrees"
        let sweptAngle = Angle.degrees (Float.int numDegrees)
        let expectedPoint = Point2d.meters expectedX expectedY
        Test.verify label Test.do
          let arc = Curve2d.arc Point2d.origin (Point2d.meters 1.0 1.0) sweptAngle
          Test.expect (Curve2d.evaluate arc 0.5 ~= expectedPoint)
  let invSqrt2 = 1.0 / Float.sqrt 2.0
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1.0 - invSqrt2)
    , testArcMidpoint -90 (1.0 - invSqrt2, invSqrt2)
    , testArcMidpoint 180 (1.0, 0.0)
    , testArcMidpoint -180 (0.0, 1.0)
    ]

arcDeformation :: Tolerance Meters => Test
arcDeformation = Test.check 100 "deformation" Test.do
  initialArc <- Random.arc2d
  transform <- Random.affineTransform2d
  t <- Parameter.random
  let transformedArc = Curve2d.transformBy transform initialArc
  let startOfTransformed = Curve2d.startPoint transformedArc
  let endOfTransformed = Curve2d.endPoint transformedArc
  let pointOnTransformed = Curve2d.evaluate transformedArc t
  let transformOfStart = Point2d.transformBy transform (Curve2d.startPoint initialArc)
  let transformOfEnd = Point2d.transformBy transform (Curve2d.endPoint initialArc)
  let transformOfPoint = Point2d.transformBy transform (Curve2d.evaluate initialArc t)
  Test.all
    [ Test.expect (startOfTransformed ~= transformOfStart)
        |> Test.output "startOfTransformed" startOfTransformed
        |> Test.output "transformOfStart" transformOfStart
    , Test.expect (endOfTransformed ~= transformOfEnd)
    , Test.expect (pointOnTransformed ~= transformOfPoint)
    ]
