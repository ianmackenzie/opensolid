module Tests.Curve2d
  ( tests
  , firstDerivativeIsConsistent
  , secondDerivativeIsConsistent
  )
where

import Angle qualified
import Arc2d qualified
import CubicSpline2d qualified
import Curve1d qualified
import Curve1d.Root qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import Curve2d.IntersectionPoint qualified as IntersectionPoint
import Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import Direction2d qualified
import DirectionCurve2d qualified
import Error qualified
import Length qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Parameter qualified
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Random (Generator)
import Random qualified
import Range (Range (Range))
import Range qualified
import Sign qualified
import Test (Expectation, Test)
import Test qualified
import Tests.Random qualified as Random
import Tolerance qualified
import Units (Meters)
import Vector2d qualified
import VectorCurve2d qualified

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
  ]

findPoint :: Tolerance Meters => Test
findPoint = Test.verify "findPoint" Test.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = QuadraticSpline2d.fromControlPoints p1 p2 p3
  startParameterValues <- Curve2d.findPoint Point2d.origin testSpline
  endParameterValues <- Curve2d.findPoint (Point2d.meters 2.0 0.0) testSpline
  midParameterValues <- Curve2d.findPoint (Point2d.meters 1.0 1.0) testSpline
  offCurveParameterValues <- Curve2d.findPoint (Point2d.meters 1.0 1.1) testSpline
  Tolerance.using 1e-12 $
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
curveOverlap1 = Test.verify "Overlap detection 1" Test.do
  let arc1 = Arc2d.from (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) Angle.halfTurn
  let arc2 = Arc2d.from (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) Angle.halfTurn
  actualSegments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.singleton (OverlappingSegment (Range.from 0.0 0.5) (Range.from 0.5 1.0) Positive)
  Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "Overlap detection 2" Test.do
  let arc1 = Arc2d.polar Point2d.origin Length.meter Angle.zero -Angle.pi
  let arc2 = Arc2d.polar Point2d.origin Length.meter (Angle.degrees -45.0) (Angle.degrees 225.0)
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.of2
          (OverlappingSegment (Range.from 0.0 (1 / 4)) (Range.from 0.0 (1 / 6)) Negative)
          (OverlappingSegment (Range.from (3 / 4) 1.0) (Range.from (5 / 6) 1.0) Negative)
  Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "Crossing intersection" Test.do
  let arc1 = Arc2d.from Point2d.origin (Point2d.meters 0.0 1.0) Angle.halfTurn
  let arc2 = Arc2d.from Point2d.origin (Point2d.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.of2
          (IntersectionPoint 0.0 0.0 IntersectionPoint.Crossing Positive)
          (IntersectionPoint 0.5 0.5 IntersectionPoint.Crossing Negative)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 (Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints))
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "Tangent intersection" Test.do
  let arc1 = Arc2d.polar Point2d.origin Length.meter Angle.zero Angle.pi
  let arc2 = Arc2d.polar (Point2d.meters 0.0 1.5) (Length.meters 0.5) -Angle.pi Angle.zero
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.singleton (IntersectionPoint 0.5 0.5 IntersectionPoint.Tangent Positive)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 (Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints))
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

solving :: Tolerance Meters => Test
solving = Test.verify "Solving via Curve1d" Test.do
  let arc = Arc2d.from (Point2d.meters 0.0 1.0) (Point2d.meters 1.0 0.0) Angle.quarterTurn
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  let desiredDistance = Length.meters 0.5
  roots <-
    Tolerance.using (Tolerance.ofSquared desiredDistance) $
      Curve1d.zeros (squaredDistanceFromOrigin - Qty.squared desiredDistance)
  let distances =
        roots
          |> List.map Curve1d.Root.value
          |> List.map (Curve2d.pointOn arc)
          |> List.map (Point2d.distanceFrom Point2d.origin)
  Test.expect (distances ~= [desiredDistance, desiredDistance])

degenerateStartPointTangent :: Tolerance Meters => Test
degenerateStartPointTangent = Test.check 100 "Degenerate start point" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = CubicSpline2d.fromControlPoints p0 p0 p1 p2
  let decreasingTValues = [2.0 ** -n | n <- [8 .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let startTangent = DirectionCurve2d.evaluateAt 0.0 tangentDirection
  let otherTangents = [DirectionCurve2d.evaluateAt t tangentDirection | t <- decreasingTValues]
  let angleDifference otherTangent = Qty.abs (Direction2d.angleFrom startTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

degenerateEndPointTangent :: Tolerance Meters => Test
degenerateEndPointTangent = Test.check 100 "Degenerate end point" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = CubicSpline2d.fromControlPoints p0 p1 p2 p2
  let increasingTValues = [1.0 - 2.0 ** -n | n <- [8 .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let endTangent = DirectionCurve2d.evaluateAt 1.0 tangentDirection
  let otherTangents = [DirectionCurve2d.evaluateAt t tangentDirection | t <- increasingTValues]
  let angleDifference otherTangent = Qty.abs (Direction2d.angleFrom endTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

tangentDerivativeIsPerpendicularToTangent :: Tolerance Meters => Test
tangentDerivativeIsPerpendicularToTangent =
  Test.check 100 "Tangent derivative is perpendicular to tangent" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    p3 <- Random.point2d
    let curve = CubicSpline2d.fromControlPoints p0 p1 p2 p3
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    t <- Parameter.random
    let tangent = DirectionCurve2d.evaluateAt t tangentDirection
    let derivative = VectorCurve2d.evaluateAt t tangentDerivative
    Test.expect (Tolerance.using 1e-12 (derivative <> tangent ~= 0.0))
      |> Test.output "t" t
      |> Test.output "tangent" tangent
      |> Test.output "derivative" derivative
      |> Test.output "dot product" (derivative <> tangent)

degenerateStartPointTangentDerivative :: Tolerance Meters => Test
degenerateStartPointTangentDerivative =
  Test.check 100 "Degenerate start point derivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = CubicSpline2d.fromControlPoints p0 p0 p1 p2
    let decreasingTValues = [2.0 ** -n | n <- [8 .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let startTangentDerivative = VectorCurve2d.evaluateAt 0.0 tangentDerivative
    let otherTangentDerivatives =
          [VectorCurve2d.evaluateAt t tangentDerivative | t <- decreasingTValues]
    let differences =
          otherTangentDerivatives
            |> List.map (- startTangentDerivative)
            |> List.map Vector2d.magnitude
    Test.expect (List.isDescending differences)

degenerateEndPointTangentDerivative :: Tolerance Meters => Test
degenerateEndPointTangentDerivative =
  Test.check 100 "Degenerate end point derivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = CubicSpline2d.fromControlPoints p0 p1 p2 p2
    let increasingTValues = [1.0 - 2.0 ** -n | n <- [8 .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let endTangentDerivative = VectorCurve2d.evaluateAt 1.0 tangentDerivative
    let otherTangentDerivatives =
          [VectorCurve2d.evaluateAt t tangentDerivative | t <- increasingTValues]
    let differences =
          otherTangentDerivatives
            |> List.map (- endTangentDerivative)
            |> List.map Vector2d.magnitude
    Test.expect (List.isDescending differences)

firstDerivativeIsConsistent :: Curve2d (space @ Meters) -> Float -> Expectation
firstDerivativeIsConsistent curve t = do
  let firstDerivative = Curve2d.derivative curve
  let dt = 1e-6
  let p1 = Curve2d.pointOn curve (t - dt)
  let p2 = Curve2d.pointOn curve (t + dt)
  let numericalFirstDerivative = (p2 - p1) / (2 * dt)
  let analyticFirstDerivative = VectorCurve2d.evaluateAt t firstDerivative
  Test.expect (Tolerance.using (Length.meters 1e-6) (numericalFirstDerivative ~= analyticFirstDerivative))
    |> Test.output "numericalFirstDerivative" numericalFirstDerivative
    |> Test.output "analyticFirstDerivative" analyticFirstDerivative

firstDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
firstDerivativeConsistency curveGenerator = Test.check 100 "First derivative" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  firstDerivativeIsConsistent curve t

secondDerivativeIsConsistent :: Curve2d (space @ Meters) -> Float -> Expectation
secondDerivativeIsConsistent curve t = do
  let firstDerivative = Curve2d.derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  let dt = 1e-6
  let v1 = VectorCurve2d.evaluateAt (t - dt) firstDerivative
  let v2 = VectorCurve2d.evaluateAt (t + dt) firstDerivative
  let numericalSecondDerivative = (v2 - v1) / (2 * dt)
  let analyticSecondDerivative = VectorCurve2d.evaluateAt t secondDerivative
  Test.expect (Tolerance.using (Length.meters 1e-6) (numericalSecondDerivative ~= analyticSecondDerivative))
    |> Test.output "numericalSecondDerivative" numericalSecondDerivative
    |> Test.output "analyticSecondDerivative" analyticSecondDerivative

secondDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
secondDerivativeConsistency curveGenerator = Test.check 100 "Second derivative" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  secondDerivativeIsConsistent curve t

derivativeConsistency :: Tolerance Meters => Test
derivativeConsistency =
  Test.group "Derivative consistency" $
    List.forEach curveGenerators $
      \(label, generator) ->
        Test.group label $
          [ firstDerivativeConsistency generator
          , secondDerivativeConsistency generator
          ]

reversalConsistency :: Tolerance Meters => Test
reversalConsistency =
  Test.group "Reversal consistency" $
    List.forEach curveGenerators $
      \(label, generator) ->
        Test.check 100 label Test.do
          curve <- generator
          let reversedCurve = Curve2d.reverse curve
          t <- Parameter.random
          Test.expect (Curve2d.pointOn curve t ~= Curve2d.pointOn reversedCurve (1 - t))

degeneracyRemoval :: Tolerance Meters => Test
degeneracyRemoval = Test.check 100 "degeneracyRemoval" Test.do
  arcCenter <- Random.point2d
  arcRadius <- Qty.random (Length.meters 0.1) (Length.meters 10.0)
  arcStartAngle <- Qty.random -Angle.pi Angle.pi
  arcSweptAngle <- Random.map (Angle.degree *) Sign.random
  let arcEndAngle = arcStartAngle + arcSweptAngle
  t <- Parameter.random

  let arc = Arc2d.polar arcCenter arcRadius arcStartAngle arcEndAngle
  let arcFirstDerivative = Curve2d.derivative arc
  let arcSecondDerivative = VectorCurve2d.derivative arcFirstDerivative
  let arcThirdDerivative = VectorCurve2d.derivative arcSecondDerivative

  let startFirstDerivative = VectorCurve2d.evaluateAt 0.0 arcFirstDerivative
  let startSecondDerivative = VectorCurve2d.evaluateAt 0.0 arcSecondDerivative
  let startCondition = (Curve2d.startPoint arc, [startFirstDerivative, startSecondDerivative])

  let interpolatedCurve = Curve2d.removeStartDegeneracy 2 startCondition arc
  let interpolatedFirstDerivative = Curve2d.derivative interpolatedCurve
  let interpolatedSecondDerivative = VectorCurve2d.derivative interpolatedFirstDerivative
  let interpolatedThirdDerivative = VectorCurve2d.derivative interpolatedSecondDerivative

  let arcPoint = Curve2d.pointOn arc t
  let interpolatedPoint = Curve2d.pointOn interpolatedCurve t
  let expectEqualPoints =
        Tolerance.using (Length.meters 1e-12) (Test.expect (arcPoint ~= interpolatedPoint))
          |> Test.output "point distance" (Point2d.distanceFrom arcPoint interpolatedPoint)
  let expectEqualDerivatives n arcDerivative interpolatedDerivative = do
        let arcEndValue = VectorCurve2d.evaluateAt 1.0 arcDerivative
        let interpolatedEndValue = VectorCurve2d.evaluateAt 1.0 interpolatedDerivative
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
