module Tests.Curve2D
  ( tests
  , firstDerivativeIsConsistent
  , firstDerivativeIsConsistentWithin
  , secondDerivativeIsConsistent
  , boundsConsistency
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import Test (Expectation, Test)
import Test qualified
import Tests.Random qualified as Random

curveGenerators :: Tolerance Meters => List (Text, Generator (Curve2D Meters space))
curveGenerators =
  [ ("Line2D", Random.line2D)
  , ("Arc2D", Random.arc2D)
  , ("QuadraticSpline2D", Random.quadraticSpline2D)
  , ("CubicSpline2D", Random.cubicSpline2D)
  ]

tests :: Tolerance Meters => List Test
tests =
  [ findPoint
  , findOwnPoint
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
  , arcConstruction
  , arcDeformation
  , g2
  ]

findPoint :: Tolerance Meters => Test
findPoint = Test.verify "findPoint" Test.do
  let p1 = Point2D.meters 0 0
  let p2 = Point2D.meters 1 2
  let p3 = Point2D.meters 2 0
  let testSpline = Curve2D.quadraticBezier p1 p2 p3
  startParameterValues <- Curve2D.findPoint Point2D.origin testSpline
  endParameterValues <- Curve2D.findPoint (Point2D.meters 2 0) testSpline
  midParameterValues <- Curve2D.findPoint (Point2D.meters 1 1) testSpline
  offCurveParameterValues <- Curve2D.findPoint (Point2D.meters 1 1.1) testSpline
  Tolerance.using 1e-12 do
    Test.all
      [ Test.expect (startParameterValues ~= [0])
          & Test.output "startParameterValues" startParameterValues
      , Test.expect (endParameterValues ~= [1])
          & Test.output "endParameterValues" endParameterValues
      , Test.expect (midParameterValues ~= [0.5])
          & Test.output "midParameterValues" midParameterValues
      , Test.expect (offCurveParameterValues == [])
          & Test.output "offCurveParameterValues" offCurveParameterValues
      ]

findOwnPoint :: Tolerance Meters => Test
findOwnPoint = Test.check 500 "findOwnPoint" Test.do
  let p1 = Point2D.meters 0 0
  let p2 = Point2D.meters 1 2
  let p3 = Point2D.meters 2 0
  let testSpline = Curve2D.quadraticBezier p1 p2 p3
  t <- Parameter.random
  let p = Curve2D.evaluate testSpline t
  solutions <- Curve2D.findPoint p testSpline
  Tolerance.using 1e-12 do
    Test.expect (solutions ~= [t])
      & Test.output "t" t
      & Test.output "solutions" solutions

overlappingSegments ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  Result Text (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 =
  case Curve2D.intersections curve1 curve2 of
    Ok (Just (Curve2D.OverlappingSegments segments)) -> Ok segments
    Ok (Just (Curve2D.IntersectionPoints _)) ->
      Error "Should have found some overlapping segments, got intersection points instead"
    Ok Nothing -> Error "Should have found some overlapping segments"
    Error error -> Error (Text.show error)

equalUBounds :: Interval Unitless -> Interval Unitless -> Bool
equalUBounds (Interval actualLow actualHigh) (Interval expectedLow expectedHigh) =
  Tolerance.using 1e-12 (actualLow ~= expectedLow && actualHigh ~= expectedHigh)

equalOverlapSegments :: OverlappingSegment -> OverlappingSegment -> Bool
equalOverlapSegments segment1 segment2 = do
  let OverlappingSegment actual1 actual2 actualSign = segment1
  let OverlappingSegment expected1 expected2 expectedSign = segment2
  equalUBounds actual1 expected1 && equalUBounds actual2 expected2 && actualSign == expectedSign

equalOverlapSegmentLists :: NonEmpty OverlappingSegment -> NonEmpty OverlappingSegment -> Bool
equalOverlapSegmentLists actualSegments expectedSegments =
  NonEmpty.length actualSegments == NonEmpty.length expectedSegments
    && NonEmpty.allTrue (NonEmpty.map2 equalOverlapSegments actualSegments expectedSegments)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "curveOverlap1" Test.do
  let arc1 = Curve2D.arcFrom (Point2D.meters 1 0) (Point2D.meters -1 0) Angle.halfTurn
  let arc2 = Curve2D.arcFrom (Point2D.meters 0 -1) (Point2D.meters 0 1) Angle.halfTurn
  actualSegments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.one (OverlappingSegment (Interval 0 0.5) (Interval 0.5 1) Positive)
  Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "curveOverlap2" Test.do
  let arc1 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle (negative Angle.pi))
  let arc2 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle (Angle.degrees -45))
          (#endAngle (Angle.degrees 225))
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.two
          (OverlappingSegment (Interval 0 (1 / 4)) (Interval 0 (1 / 6)) Negative)
          (OverlappingSegment (Interval (3 / 4) 1) (Interval (5 / 6) 1) Negative)
  Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "crossingIntersection" Test.do
  let arc1 = Curve2D.arcFrom Point2D.origin (Point2D.meters 0 1) Angle.halfTurn
  let arc2 = Curve2D.arcFrom Point2D.origin (Point2D.meters 1 0) (negative Angle.halfTurn)
  intersections <- Curve2D.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.two
          (IntersectionPoint.crossing 0 0 Positive)
          (IntersectionPoint.crossing 0.5 0.5 Negative)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2D.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 (Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints))
        & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
        & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just (Curve2D.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "tangentIntersection" Test.do
  let arc1 =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle Angle.pi)
  let arc2 =
        Curve2D.polarArc
          (#centerPoint (Point2D.meters 0 1.5))
          (#radius (Length.meters 0.5))
          (#startAngle (negative Angle.pi))
          (#endAngle Angle.zero)
  intersections <- Curve2D.intersections arc1 arc2
  let expectedIntersectionPoints = NonEmpty.one (IntersectionPoint.tangent 0.5 0.5)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2D.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 do
        Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints)
          & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
          & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just (Curve2D.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

solving :: Tolerance Meters => Test
solving = Test.verify "solving" Test.do
  let arc = Curve2D.arcFrom (Point2D.meters 0 1) (Point2D.meters 1 0) Angle.quarterTurn
  let distanceFromOrigin = VectorCurve2D.magnitude (arc .-. Point2D.origin)
  let desiredDistance = Length.meters 0.5
  zeros <- Curve.zeros (distanceFromOrigin .-. desiredDistance)
  let distanceAt zero = Point2D.distanceFrom Point2D.origin (Curve2D.evaluate arc zero.location)
  Test.expect (List.map distanceAt zeros ~= [desiredDistance, desiredDistance])

degenerateStartPointTangent :: Tolerance Meters => Test
degenerateStartPointTangent = Test.check 100 "degenerateStartPointTangent" Test.do
  p0 <- Random.point2D
  p1 <- Random.point2D
  p2 <- Random.point2D
  let curve = Curve2D.cubicBezier p0 p0 p1 p2
  let decreasingTValues = [Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
  tangentDirection <- Curve2D.tangentDirection curve
  let startTangent = DirectionCurve2D.startValue tangentDirection
  let otherTangents = List.map (DirectionCurve2D.evaluate tangentDirection) decreasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2D.angleFrom startTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

degenerateEndPointTangent :: Tolerance Meters => Test
degenerateEndPointTangent = Test.check 100 "degenerateEndPointTangent" Test.do
  p0 <- Random.point2D
  p1 <- Random.point2D
  p2 <- Random.point2D
  let curve = Curve2D.cubicBezier p0 p1 p2 p2
  let increasingTValues = [1 -. Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
  tangentDirection <- Curve2D.tangentDirection curve
  let endTangent = DirectionCurve2D.endValue tangentDirection
  let otherTangents = List.map (DirectionCurve2D.evaluate tangentDirection) increasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2D.angleFrom endTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

tangentDerivativeIsPerpendicularToTangent :: Tolerance Meters => Test
tangentDerivativeIsPerpendicularToTangent =
  Test.check 100 "tangentDerivativeIsPerpendicularToTangent" Test.do
    p0 <- Random.point2D
    p1 <- Random.point2D
    p2 <- Random.point2D
    p3 <- Random.point2D
    let curve = Curve2D.cubicBezier p0 p1 p2 p3
    tangentDirection <- Curve2D.tangentDirection curve
    let tangentDerivative = DirectionCurve2D.derivative tangentDirection
    tValue <- Parameter.random
    let tangent = DirectionCurve2D.evaluate tangentDirection tValue
    let derivative = VectorCurve2D.evaluate tangentDerivative tValue
    Test.expect (Tolerance.using 1e-12 (derivative `dot` tangent ~= 0))
      & Test.output "tValue" tValue
      & Test.output "tangent" tangent
      & Test.output "derivative" derivative
      & Test.output "dot product" (derivative `dot` tangent)

degenerateStartPointTangentDerivative :: Tolerance Meters => Test
degenerateStartPointTangentDerivative =
  Test.check 100 "degenerateStartPointTangentDerivative" Test.do
    p0 <- Random.point2D
    p1 <- Random.point2D
    p2 <- Random.point2D
    let curve = Curve2D.cubicBezier p0 p0 p1 p2
    let decreasingTValues = [Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
    tangentDirection <- Curve2D.tangentDirection curve
    let tangentDerivative = DirectionCurve2D.derivative tangentDirection
    let startTangentDerivative = VectorCurve2D.startValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2D.evaluate tangentDerivative) decreasingTValues
    let differences =
          List.map Vector2D.magnitude $
            List.map (.-. startTangentDerivative) otherTangentDerivatives
    Test.expect (List.isDescending differences)
      & Test.output "differences" differences
      & Test.output "startTangentDerivative" startTangentDerivative

degenerateEndPointTangentDerivative :: Tolerance Meters => Test
degenerateEndPointTangentDerivative =
  Test.check 100 "degenerateEndPointTangentDerivative" Test.do
    p0 <- Random.point2D
    p1 <- Random.point2D
    p2 <- Random.point2D
    let curve = Curve2D.cubicBezier p0 p1 p2 p2
    let increasingTValues = [1 -. Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
    tangentDirection <- Curve2D.tangentDirection curve
    let tangentDerivative = DirectionCurve2D.derivative tangentDirection
    let endTangentDerivative = VectorCurve2D.endValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2D.evaluate tangentDerivative) increasingTValues
    let differences =
          List.map Vector2D.magnitude $
            List.map (.-. endTangentDerivative) otherTangentDerivatives
    Test.expect (List.isDescending differences)
      & Test.output "differences" differences
      & Test.output "endTangentDerivative" endTangentDerivative

firstDerivativeIsConsistent :: Curve2D Meters space -> Number -> Expectation
firstDerivativeIsConsistent = firstDerivativeIsConsistentWithin (Length.meters 1e-6)

firstDerivativeIsConsistentWithin ::
  Show (Quantity units) =>
  Quantity units ->
  Curve2D units space ->
  Number ->
  Expectation
firstDerivativeIsConsistentWithin givenTolerance curve tValue = do
  let dt :: Number = 1e-6
  let p1 = Curve2D.evaluate curve (tValue .-. dt)
  let p2 = Curve2D.evaluate curve (tValue .+. dt)
  let numericalFirstDerivative = (p2 .-. p1) ./ (2 *. dt)
  let analyticFirstDerivative = VectorCurve2D.evaluate (Curve2D.derivative curve) tValue
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      & Test.output "numericalFirstDerivative" numericalFirstDerivative
      & Test.output "analyticFirstDerivative" analyticFirstDerivative

firstDerivativeConsistency :: Generator (Curve2D Meters space) -> Test
firstDerivativeConsistency curveGenerator = Test.check 100 "firstDerivativeConsistency" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  firstDerivativeIsConsistent curve t

secondDerivativeIsConsistent :: Curve2D Meters space -> Number -> Expectation
secondDerivativeIsConsistent curve tValue = do
  let dt :: Number = 1e-6
  let firstDerivative = Curve2D.derivative curve
  let secondDerivative = VectorCurve2D.derivative firstDerivative
  let v1 = VectorCurve2D.evaluate firstDerivative (tValue .-. dt)
  let v2 = VectorCurve2D.evaluate firstDerivative (tValue .+. dt)
  let numericalSecondDerivative = (v2 .-. v1) ./. (2 *. dt)
  let analyticSecondDerivative = VectorCurve2D.evaluate secondDerivative tValue
  Tolerance.using Length.micrometer do
    Test.expect (numericalSecondDerivative ~= analyticSecondDerivative)
      & Test.output "numericalSecondDerivative" numericalSecondDerivative
      & Test.output "analyticSecondDerivative" analyticSecondDerivative

secondDerivativeConsistency :: Generator (Curve2D Meters space) -> Test
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
          let reversedCurve = Curve2D.reverse curve
          t <- Parameter.random
          Test.expect (Curve2D.evaluate curve t ~= Curve2D.evaluate reversedCurve (1 -. t))

boundsConsistency ::
  (Tolerance units, Show (Quantity units)) =>
  Curve2D units space ->
  Expectation
boundsConsistency curve = Test.do
  tBounds <- Interval.random Parameter.random
  tValue <- Random.map (Interval.interpolate tBounds) Parameter.random
  let curveValue = Curve2D.evaluate curve tValue
  let curveBounds = Curve2D.evaluateBounds curve tBounds
  Test.expect (curveValue `intersects` curveBounds)
    & Test.output "tValue" tValue
    & Test.output "tBounds" tBounds
    & Test.output "curveValue" curveValue
    & Test.output "curveBounds" curveBounds

arcConstruction :: Tolerance Meters => Test
arcConstruction = do
  let testArcMidpoint numDegrees (expectedX, expectedY) = do
        let label = Text.int numDegrees <> " degrees"
        let sweptAngle = Angle.degrees (Number.fromInt numDegrees)
        let expectedPoint = Point2D.meters expectedX expectedY
        Test.verify label Test.do
          let arc = Curve2D.arcFrom Point2D.origin (Point2D.meters 1 1) sweptAngle
          Test.expect (Curve2D.evaluate arc 0.5 ~= expectedPoint)
  let invSqrt2 = 1 /. Number.sqrt 2
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1 -. invSqrt2)
    , testArcMidpoint -90 (1 -. invSqrt2, invSqrt2)
    , testArcMidpoint 180 (1, 0)
    , testArcMidpoint -180 (0, 1)
    ]

arcDeformation :: Tolerance Meters => Test
arcDeformation = Test.check 100 "deformation" Test.do
  initialArc <- Random.arc2D
  transform <- Random.affineTransform2D
  t <- Parameter.random
  let transformedArc = Curve2D.transformBy transform initialArc
  let pointOnTransformed = Curve2D.evaluate transformedArc t
  let transformOfStart = Point2D.transformBy transform initialArc.startPoint
  let transformOfEnd = Point2D.transformBy transform initialArc.endPoint
  let transformOfPoint = Point2D.transformBy transform (Curve2D.evaluate initialArc t)
  Test.all
    [ Test.expect (transformedArc.startPoint ~= transformOfStart)
        & Test.output "transformedArc.startPoint" transformedArc.startPoint
        & Test.output "transformOfStart" transformOfStart
    , Test.expect (transformedArc.endPoint ~= transformOfEnd)
    , Test.expect (pointOnTransformed ~= transformOfPoint)
    ]

g2 :: Tolerance Meters => Test
g2 = Test.check 100 "G2 continuity" Test.do
  p1 <- Random.point2D
  p2 <- Random.point2D
  p3 <- Random.point2D
  p4 <- Random.point2D
  let spline = Curve2D.cubicBezier p1 p2 p3 p4
  t <- Parameter.random
  let point = Curve2D.evaluate spline t
  tangentCurve <- Curve2D.tangentDirection spline
  curvatureCurve <- Curve2D.curvature spline
  let tangentDirection = DirectionCurve2D.evaluate tangentCurve t
  let signedRadius = 1 /. Curve.evaluate curvatureCurve t
  let normalDirection = Direction2D.rotateLeft tangentDirection
  let arcCenter = point .+. signedRadius .*. normalDirection
  let arc = Curve2D.sweptArc arcCenter point (Angle.degrees 30)
  Test.expect (Curve2D.g2 (spline, t) (arc, 0) Length.meter)
