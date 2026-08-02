module Tests.Curve2D
  ( tests
  , firstDerivativeIsConsistent
  , firstDerivativeIsConsistentWithin
  , secondDerivativeIsConsistent
  , rangeConsistency
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.Nonzero qualified as Curve2D.Nonzero
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import Test (Expectation, Test)
import Test qualified
import Tests.Matching (matching)
import Tests.Random qualified as Random

curveGenerators :: List (Text, Generator (Curve2D Meters))
curveGenerators =
  [ ("Line2D", Random.line2D)
  , ("Arc2D", Random.arc2D)
  , ("QuadraticSpline2D", Random.quadraticSpline2D)
  , ("CubicSpline2D", Random.cubicSpline2D)
  , ("Involute2D", Random.involute2D)
  ]

tests :: List Test
tests =
  [ findPoint
  , findOwnPoint
  , curveOverlap1
  , curveOverlap2
  , overlapAndJoin
  , crossingIntersection
  , tangentIntersection
  , degenerateStartPointTangent
  , degenerateEndPointTangent
  , derivativeConsistency
  , reversalConsistency
  , arcConstruction
  , arcDeformation
  , g2
  ]

findParameterValues ::
  Tolerance units =>
  Point2D units ->
  Curve2D units ->
  Result Text (List Number)
findParameterValues point curve = do
  curvePoints <- Curve2D.findPoint point curve ?? fail
  Ok (List.map CurvePoint.parameterValue curvePoints)

findPoint :: Test
findPoint = Test.verify "findPoint" do
  let p1 = Point2D.meters 0.0 0.0
  let p2 = Point2D.meters 1.0 2.0
  let p3 = Point2D.meters 2.0 0.0
  let testSpline = Curve2D.quadraticBezier p1 p2 p3
  startParameterValues <- findParameterValues Point2D.origin testSpline ?? fail
  endParameterValues <- findParameterValues (Point2D.meters 2.0 0.0) testSpline ?? fail
  midParameterValues <- findParameterValues (Point2D.meters 1.0 1.0) testSpline ?? fail
  offCurveParameterValues <- findParameterValues (Point2D.meters 1.0 1.1) testSpline ?? fail
  Tolerance.using 1e-12 do
    Test.all
      [ Test.expect (startParameterValues ~= [0.0])
          & Test.output "startParameterValues" startParameterValues
      , Test.expect (endParameterValues ~= [1.0])
          & Test.output "endParameterValues" endParameterValues
      , Test.expect (midParameterValues ~= [0.5])
          & Test.output "midParameterValues" midParameterValues
      , Test.expect (offCurveParameterValues == [])
          & Test.output "offCurveParameterValues" offCurveParameterValues
      ]

findOwnPoint :: Test
findOwnPoint = Test.check 500 "findOwnPoint" do
  let p1 = Point2D.meters 0.0 0.0
  let p2 = Point2D.meters 1.0 2.0
  let p3 = Point2D.meters 2.0 0.0
  let testSpline = Curve2D.quadraticBezier p1 p2 p3
  t <- Test.generate Parameter.random
  let p = Curve2D.pointAt t testSpline
  solutions <- findParameterValues p testSpline ?? fail
  Tolerance.using 1e-12 do
    Test.expect (solutions ~= [t])
      & Test.output "t" t
      & Test.output "solutions" solutions

overlappingSegments ::
  Tolerance Meters =>
  Curve2D Meters ->
  Curve2D Meters ->
  Result Text (Sign, NonEmpty (Interval Unitless, Interval Unitless), List (Curve2D.IntersectionPoint Meters))
overlappingSegments curve1 curve2 =
  case Curve2D.intersections curve1 curve2 of
    Ok (Just (Curve.OverlappingSegments sign segments intersectionPoints)) ->
      Ok (sign, segments, intersectionPoints)
    Ok (Just (Curve.IntersectionPoints _)) ->
      Err "Should have found some overlapping segments, got intersection points instead"
    Ok Nothing -> Err "Should have found some overlapping segments"
    Err err -> Err (Text.show err)

equalParameterRanges :: Interval Unitless -> Interval Unitless -> Bool
equalParameterRanges (Interval actualLow actualHigh) (Interval expectedLow expectedHigh) =
  Tolerance.using 1e-12 (actualLow ~= expectedLow && actualHigh ~= expectedHigh)

equalOverlapSegments ::
  ((Interval Unitless, Interval Unitless), (Interval Unitless, Interval Unitless)) ->
  Bool
equalOverlapSegments ((actual1, actual2), (expected1, expected2)) =
  equalParameterRanges actual1 expected1 && equalParameterRanges actual2 expected2

equalOverlapSegmentLists ::
  NonEmpty (Interval Unitless, Interval Unitless) ->
  NonEmpty (Interval Unitless, Interval Unitless) ->
  Bool
equalOverlapSegmentLists actualSegments expectedSegments =
  NonEmpty.length actualSegments == NonEmpty.length expectedSegments
    && NonEmpty.all equalOverlapSegments (NonEmpty.zip2 actualSegments expectedSegments)

curveOverlap1 :: Test
curveOverlap1 = Test.verify "curveOverlap1" do
  let arc1 = Curve2D.arcFrom (Point2D.meters 1.0 0.0) (Point2D.meters -1.0 0.0) Angle.halfTurn
  let arc2 = Curve2D.arcFrom (Point2D.meters 0.0 -1.0) (Point2D.meters 0.0 1.0) Angle.halfTurn
  (sign, actualSegments, points) <- overlappingSegments arc1 arc2 ?? fail
  let expectedSegments = NonEmpty.one (Interval 0.0 0.5, Interval 0.5 1.0)
  Test.all
    [ Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)
    , Test.expect (sign == Positive)
    , Test.expect (List.isEmpty points)
    ]

curveOverlap2 :: Test
curveOverlap2 = Test.verify "curveOverlap2" do
  let arc1 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle -Angle.pi)
  let arc2 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle (Angle.degrees -45.0))
          (#endAngle (Angle.degrees 225.0))
  (sign, segments, points) <- overlappingSegments arc1 arc2 ?? fail
  let expectedSegments =
        NonEmpty.two
          (Interval 0.0 (1 / 4), Interval 0.0 (1 / 6))
          (Interval (3 / 4) 1.0, Interval (5 / 6) 1.0)
  Test.all
    [ Test.expect (equalOverlapSegmentLists segments expectedSegments)
    , Test.expect (sign == Negative)
    , Test.expect (List.isEmpty points)
    ]

overlapAndJoin :: Test
overlapAndJoin = Test.verify "overlapAndJoin" do
  let arc1 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle -Angle.pi)
  let arc2 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle (Angle.degrees -45.0))
          (#endAngle Angle.pi)
  nondegenerate1 <- Curve.nondegenerate arc1 ?? fail
  nondegenerate2 <- Curve.nondegenerate arc2 ?? fail
  let curvePoint1 t1 = Curve.Nondegenerate.curvePointAt t1 nondegenerate1
  let curvePoint2 t2 = Curve.Nondegenerate.curvePointAt t2 nondegenerate2
  (sign, segments, points) <- overlappingSegments arc1 arc2 ?? fail
  let expectedSegments = NonEmpty.one (Interval 0.0 (1 / 4), Interval 0.0 (1 / 5))
  let expectedPoints =
        [IntersectionPoint.indistinguishable Negative (curvePoint1 1.0, curvePoint2 1.0)]
  Test.all
    [ Test.expect (equalOverlapSegmentLists segments expectedSegments)
        & Test.output "segments" segments
        & Test.output "expectedSegments" expectedSegments
    , Test.expect (sign == Negative)
    , Test.expect (points `matching` expectedPoints)
        & Test.output "points" (List.map IntersectionPoint.parameterValues points)
        & Test.output "expectedPoints" (List.map IntersectionPoint.parameterValues expectedPoints)
    ]

crossingIntersection :: Test
crossingIntersection = Test.verify "crossingIntersection" do
  let arc1 = Curve2D.arcFrom Point2D.origin (Point2D.meters 0.0 1.0) Angle.halfTurn
  let arc2 = Curve2D.arcFrom Point2D.origin (Point2D.meters 1.0 0.0) -Angle.halfTurn
  nondegenerate1 <- Curve.nondegenerate arc1 ?? fail
  nondegenerate2 <- Curve.nondegenerate arc2 ?? fail
  let curvePoint1 t1 = Curve.Nondegenerate.curvePointAt t1 nondegenerate1
  let curvePoint2 t2 = Curve.Nondegenerate.curvePointAt t2 nondegenerate2
  intersections <- Curve2D.intersections arc1 arc2 ?? fail
  let expectedIntersectionPoints =
        NonEmpty.two
          (IntersectionPoint.crossing (curvePoint1 0.0, curvePoint2 0.0))
          (IntersectionPoint.crossing (curvePoint1 0.5, curvePoint2 0.5))
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (matching actualIntersectionPoints expectedIntersectionPoints)
        & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
        & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just Curve.OverlappingSegments{} ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Test
tangentIntersection = Test.verify "tangentIntersection" do
  let arc1 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle Angle.pi)
  let arc2 =
        Curve2D.polarArc
          (#centerPoint (Point2D.meters 0.0 1.5))
          (#radius (Length.meters 0.5))
          (#startAngle -Angle.pi)
          (#endAngle Angle.zero)
  nondegenerate1 <- Curve.nondegenerate arc1 ?? fail
  nondegenerate2 <- Curve.nondegenerate arc2 ?? fail
  let curvePoint1 t1 = Curve.Nondegenerate.curvePointAt t1 nondegenerate1
  let curvePoint2 t2 = Curve.Nondegenerate.curvePointAt t2 nondegenerate2
  intersections <- Curve2D.intersections arc1 arc2 ?? fail
  let expectedIntersectionPoints =
        NonEmpty.one (IntersectionPoint.tangent Negative (curvePoint1 0.5, curvePoint2 0.5))
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (matching actualIntersectionPoints expectedIntersectionPoints)
        & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
        & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just Curve.OverlappingSegments{} ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

degenerateStartPointTangent :: Test
degenerateStartPointTangent = Test.check 100 "degenerateStartPointTangent" do
  p0 <- Test.generate Random.point2D
  p1 <- Test.generate Random.point2D
  p2 <- Test.generate Random.point2D
  curve <- Curve.nondegenerate (Curve2D.cubicBezier p0 p0 p1 p2) ?? fail
  let decreasingTValues = [2.0 ** Number.fromInt -n | n <- [8 .. 16]]
  let tangentDirectionAt tValue = Curve.Nondegenerate.tangentDirectionAt tValue curve
  let startTangent = tangentDirectionAt 0.0
  let otherTangents = List.map tangentDirectionAt decreasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2D.angleFrom startTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

degenerateEndPointTangent :: Test
degenerateEndPointTangent = Test.check 100 "degenerateEndPointTangent" do
  p0 <- Test.generate Random.point2D
  p1 <- Test.generate Random.point2D
  p2 <- Test.generate Random.point2D
  curve <- Curve.nondegenerate (Curve2D.cubicBezier p0 p1 p2 p2) ?? fail
  let increasingTValues = [1.0 - 2.0 ** Number.fromInt -n | n <- [8 .. 16]]
  let tangentDirectionAt tValue = Curve.Nondegenerate.tangentDirectionAt tValue curve
  let endTangent = tangentDirectionAt 1.0
  let otherTangents = List.map tangentDirectionAt increasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2D.angleFrom endTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

firstDerivativeIsConsistent :: Curve2D Meters -> Number -> Expectation
firstDerivativeIsConsistent = firstDerivativeIsConsistentWithin (Length.meters 1e-6)

firstDerivativeIsConsistentWithin ::
  Show (Quantity units) =>
  Quantity units ->
  Curve2D units ->
  Number ->
  Expectation
firstDerivativeIsConsistentWithin givenTolerance curve tValue = do
  let dt :: Number = 1e-6
  let p1 = Curve2D.pointAt (tValue - dt) curve
  let p2 = Curve2D.pointAt (tValue + dt) curve
  let numericalFirstDerivative = (p2 - p1) / (2.0 * dt)
  let analyticFirstDerivative = Curve2D.derivativeAt tValue curve
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      & Test.output "numericalFirstDerivative" numericalFirstDerivative
      & Test.output "analyticFirstDerivative" analyticFirstDerivative

firstDerivativeConsistency :: Generator (Curve2D Meters) -> Test
firstDerivativeConsistency randomCurve = Test.check 100 "firstDerivativeConsistency" do
  curve <- Test.generate randomCurve
  t <- Test.generate Parameter.random
  firstDerivativeIsConsistent curve t

secondDerivativeIsConsistent :: Curve2D Meters -> Number -> Expectation
secondDerivativeIsConsistent curve tValue = do
  let dt :: Number = 1e-6
  let v1 = Curve2D.derivativeAt (tValue - dt) curve
  let v2 = Curve2D.derivativeAt (tValue + dt) curve
  let numericalSecondDerivative = (v2 - v1) / (2.0 * dt)
  let analyticSecondDerivative = Curve2D.secondDerivativeAt tValue curve
  Tolerance.using Length.micrometer do
    Test.expect (numericalSecondDerivative ~= analyticSecondDerivative)
      & Test.output "numericalSecondDerivative" numericalSecondDerivative
      & Test.output "analyticSecondDerivative" analyticSecondDerivative

secondDerivativeConsistency :: Generator (Curve2D Meters) -> Test
secondDerivativeConsistency randomCurve = Test.check 100 "secondDerivativeConsistency" do
  curve <- Test.generate randomCurve
  t <- Test.generate Parameter.random
  secondDerivativeIsConsistent curve t

derivativeConsistency :: Test
derivativeConsistency =
  Test.group "derivativeConsistency" $
    curveGenerators & List.map
      \(label, generator) ->
        Test.group label $
          [ firstDerivativeConsistency generator
          , secondDerivativeConsistency generator
          ]

reversalConsistency :: Test
reversalConsistency =
  Test.group "reversalConsistency" $
    curveGenerators & List.map
      \(label, randomCurve) ->
        Test.check 100 label do
          curve <- Test.generate randomCurve
          let reversedCurve = Curve2D.reverse curve
          t <- Test.generate Parameter.random
          Test.expect (Curve2D.pointAt t curve ~= Curve2D.pointAt (1.0 - t) reversedCurve)

rangeConsistency :: (Tolerance units, Show (Quantity units)) => Curve2D units -> Expectation
rangeConsistency curve = do
  tRange <- Test.generate (Interval.random Parameter.random)
  tValue <- Test.generate (Random.map (Interval.interpolate tRange) Parameter.random)
  let curveValue = Curve2D.pointAt tValue curve
  let curveRange = Curve2D.range tRange curve
  Test.expect (curveValue `intersects` curveRange)
    & Test.output "tValue" tValue
    & Test.output "tRange" tRange
    & Test.output "curveValue" curveValue
    & Test.output "curveRange" curveRange

arcConstruction :: Test
arcConstruction = do
  let testArcMidpoint numDegrees (expectedX, expectedY) = do
        let label = Text.int numDegrees <> " degrees"
        let sweptAngle = Angle.degrees (Number.fromInt numDegrees)
        let expectedPoint = Point2D.meters expectedX expectedY
        Test.verify label do
          let arc = Curve2D.arcFrom Point2D.origin (Point2D.meters 1.0 1.0) sweptAngle
          Test.expect (Curve2D.pointAt 0.5 arc ~= expectedPoint)
  let invSqrt2 = 1.0 / Number.sqrt 2.0
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1.0 - invSqrt2)
    , testArcMidpoint -90 (1.0 - invSqrt2, invSqrt2)
    , testArcMidpoint 180 (1.0, 0.0)
    , testArcMidpoint -180 (0.0, 1.0)
    ]

arcDeformation :: Test
arcDeformation = Test.check 100 "deformation" do
  initialArc <- Test.generate Random.arc2D
  transform <- Test.generate Random.affineTransform2D
  t <- Test.generate Parameter.random
  let transformedArc = Curve2D.transformBy transform initialArc
  let pointOnTransformed = Curve2D.pointAt t transformedArc
  let transformOfStart = Point2D.transformBy transform (Curve2D.startPoint initialArc)
  let transformOfEnd = Point2D.transformBy transform (Curve2D.endPoint initialArc)
  let transformOfPoint = Point2D.transformBy transform (Curve2D.pointAt t initialArc)
  Test.all
    [ Test.expect (Curve2D.startPoint transformedArc ~= transformOfStart)
        & Test.output "transformedArc.startPoint" (Curve2D.startPoint transformedArc)
        & Test.output "transformOfStart" transformOfStart
    , Test.expect (Curve2D.endPoint transformedArc ~= transformOfEnd)
    , Test.expect (pointOnTransformed ~= transformOfPoint)
    ]

g2 :: Test
g2 = Test.check 100 "G2 continuity" do
  p1 <- Test.generate Random.point2D
  p2 <- Test.generate Random.point2D
  p3 <- Test.generate Random.point2D
  p4 <- Test.generate Random.point2D
  spline <- Curve.nonzero (Curve2D.cubicBezier p1 p2 p3 p4) ?? fail
  t <- Test.generate Parameter.random
  let point = Curve2D.Nonzero.pointAt t spline
  let tangentDirection = Curve.Nonzero.tangentDirectionAt t spline
  let curvatureVector = Curve.Nonzero.curvatureVectorAt t spline
  let signedRadius = 1.0 / (tangentDirection `cross` curvatureVector)
  let normalDirection = Direction2D.rotateLeft tangentDirection
  let arcCenter = point + signedRadius * normalDirection
  let arc = Curve2D.sweptArc arcCenter point (Quantity.sign signedRadius * Angle.degrees 30.0)
  nondegenerateArc <- Curve.nondegenerate arc ?? fail
  let splinePoint = Curve.Nondegenerate.curvePointAt t (Nondegenerate.fromNonzero spline)
  let arcPoint = Curve.Nondegenerate.curvePointAt 0.0 nondegenerateArc
  let continuity = CurvePoint.continuity splinePoint arcPoint
  Test.expect (continuity == Just (Continuity.Indistinguishable Positive))
    & Test.output "continuity" continuity
