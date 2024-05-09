module Tests.Curve2d (tests) where

import Angle qualified
import Arc2d qualified
import CubicSpline2d qualified
import Curve1d qualified
import Curve1d.Root qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Direction2d qualified
import DirectionCurve2d qualified
import Error qualified
import Length qualified
import List qualified
import OpenSolid
import Parameter qualified
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Random (Generator)
import Range (Range (Range))
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Tolerance qualified
import Units (Meters)
import Vector2d qualified
import VectorCurve2d qualified

curveGenerators :: Tolerance Meters => List (String, Generator (Curve2d (space @ Meters)))
curveGenerators =
  [ ("Line2d", Random.line2d)
  , ("Arc2d", Random.arc2d)
  , ("QuadraticSpline2d", Random.quadraticSpline2d)
  , ("CubicSpline2d", Random.cubicSpline2d)
  ]

tests :: Tolerance Meters => List Test
tests =
  [ find
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
  ]

find :: Tolerance Meters => Test
find = Test.verify "find" Test.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = QuadraticSpline2d.fromControlPoints p1 p2 p3
  let startParameterValues = Curve2d.find Point2d.origin testSpline
  let endParameterValues = Curve2d.find (Point2d.meters 2.0 0.0) testSpline
  let midParameterValues = Curve2d.find (Point2d.meters 1.0 1.0) testSpline
  let offCurveParameterValues = Curve2d.find (Point2d.meters 1.0 1.1) testSpline
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
  Result String (List (Range Unitless, Range Unitless, Sign))
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok _ -> Error "Intersection should have failed (and given overlapping segments)"
    Error (Curve2d.CurvesOverlap segments) -> Ok segments
    Error error -> Error (Error.message error)

equalUBounds :: Range Unitless -> Range Unitless -> Bool
equalUBounds (Range actualLow actualHigh) (Range expectedLow expectedHigh) =
  Tolerance.using 1e-12 (actualLow ~= expectedLow && actualHigh ~= expectedHigh)

equalOverlapSegments :: (Range Unitless, Range Unitless, Sign) -> (Range Unitless, Range Unitless, Sign) -> Bool
equalOverlapSegments (actual1, actual2, actualSign) (expected1, expected2, expectedSign) =
  equalUBounds actual1 expected1 && equalUBounds actual2 expected2 && actualSign == expectedSign

equalOverlapSegmentLists :: List (Range Unitless, Range Unitless, Sign) -> List (Range Unitless, Range Unitless, Sign) -> Bool
equalOverlapSegmentLists actualSegments expectedSegments =
  List.allTrue (List.map2 equalOverlapSegments actualSegments expectedSegments)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "Overlap detection 1" Test.do
  let arc1 = Arc2d.from (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) Angle.halfTurn
  let arc2 = Arc2d.from (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) Angle.halfTurn
  actualSegments <- overlappingSegments arc1 arc2
  let expectedSegments = [(Range.from 0.0 0.5, Range.from 0.5 1.0, Positive)]
  Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "Overlap detection 2" Test.do
  arc1 <-
    Arc2d.build
      ( Arc2d.centerPoint Point2d.origin
      , Arc2d.radius (Length.meters 1.0)
      , Arc2d.startAngle (Angle.degrees 0.0)
      , Arc2d.endAngle (Angle.degrees -180.0)
      )
  arc2 <-
    Arc2d.build
      ( Arc2d.centerPoint Point2d.origin
      , Arc2d.radius (Length.meters 1.0)
      , Arc2d.startAngle (Angle.degrees -45.0)
      , Arc2d.endAngle (Angle.degrees 225.0)
      )
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        [ (Range.from 0.0 (1 / 4), Range.from 0.0 (1 / 6), Negative)
        , (Range.from (3 / 4) 1.0, Range.from (5 / 6) 1.0, Negative)
        ]
  Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "Crossing intersection" Test.do
  let arc1 = Arc2d.from Point2d.origin (Point2d.meters 0.0 1.0) Angle.halfTurn
  let arc2 = Arc2d.from Point2d.origin (Point2d.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersections =
        [ Intersection 0.0 0.0 Intersection.Crossing Positive
        , Intersection 0.5 0.5 Intersection.Crossing Negative
        ]
  Tolerance.using 1e-12 (Test.expect (intersections ~= expectedIntersections))

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "Tangent intersection" Test.do
  arc1 <-
    Arc2d.build
      ( Arc2d.centerPoint Point2d.origin
      , Arc2d.radius Length.meter
      , Arc2d.startAngle (Angle.degrees 0.0)
      , Arc2d.endAngle (Angle.degrees 180.0)
      )
  arc2 <-
    Arc2d.build
      ( Arc2d.centerPoint (Point2d.meters 0.0 1.5)
      , Arc2d.radius (Length.meters 0.5)
      , Arc2d.startAngle (Angle.degrees -180.0)
      , Arc2d.endAngle (Angle.degrees 0.0)
      )
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersections = [Intersection 0.5 0.5 Intersection.Tangent Positive]
  Tolerance.using 1e-12 (Test.expect (intersections ~= expectedIntersections))

solving :: Tolerance Meters => Test
solving = Test.verify "Solving via Curve1d" Test.do
  let arc = Arc2d.from (Point2d.meters 0.0 1.0) (Point2d.meters 1.0 0.0) Angle.quarterTurn
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  let desiredDistance = Length.meters 0.5
  roots <-
    Tolerance.using (Tolerance.ofSquared desiredDistance) $
      case Curve1d.zeros (squaredDistanceFromOrigin - Qty.squared desiredDistance) of
        Curve1d.ZeroEverywhere -> Error "Internal error in test, curve should not be zero everywhere"
        Curve1d.Zeros roots -> Ok roots
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

firstDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
firstDerivativeConsistency curveGenerator = Test.check 100 "First derivative" Test.do
  curve <- curveGenerator
  let firstDerivative = Curve2d.derivative curve
  t <- Parameter.random
  let dt = 1e-6
  let numericalFirstDerivative = (Curve2d.evaluateAt (t + dt) curve - Curve2d.evaluateAt (t - dt) curve) / (2 * dt)
  let analyticFirstDerivative = VectorCurve2d.evaluateAt t firstDerivative
  Test.expect (Tolerance.using (Length.meters 1e-6) (numericalFirstDerivative ~= analyticFirstDerivative))

secondDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
secondDerivativeConsistency curveGenerator = Test.check 100 "Second derivative" Test.do
  curve <- curveGenerator
  let firstDerivative = Curve2d.derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  t <- Parameter.random
  let dt = 1e-6
  let numericalSecondDerivative = (VectorCurve2d.evaluateAt (t + dt) firstDerivative - VectorCurve2d.evaluateAt (t - dt) firstDerivative) / (2 * dt)
  let analyticSecondDerivative = VectorCurve2d.evaluateAt t secondDerivative
  Test.expect (Tolerance.using (Length.meters 1e-6) (numericalSecondDerivative ~= analyticSecondDerivative))

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
          Test.expect (Curve2d.evaluateAt t curve ~= Curve2d.evaluateAt (1 - t) reversedCurve)
