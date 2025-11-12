module Tests.Curve2d
  ( tests
  , firstDerivativeIsConsistent
  , firstDerivativeIsConsistentWithin
  , secondDerivativeIsConsistent
  , boundsConsistency
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
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
  let p1 = Point2d.meters 0 0
  let p2 = Point2d.meters 1 2
  let p3 = Point2d.meters 2 0
  let testSpline = Curve2d.quadraticBezier p1 p2 p3
  startParameterValues <- Curve2d.findPoint Point2d.origin testSpline
  endParameterValues <- Curve2d.findPoint (Point2d.meters 2 0) testSpline
  midParameterValues <- Curve2d.findPoint (Point2d.meters 1 1) testSpline
  offCurveParameterValues <- Curve2d.findPoint (Point2d.meters 1 1.1) testSpline
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
  let p1 = Point2d.meters 0 0
  let p2 = Point2d.meters 1 2
  let p3 = Point2d.meters 2 0
  let testSpline = Curve2d.quadraticBezier p1 p2 p3
  t <- Parameter.random
  let p = Curve2d.evaluate testSpline t
  solutions <- Curve2d.findPoint p testSpline
  Tolerance.using 1e-12 do
    Test.expect (solutions ~= [t])
      & Test.output "t" t
      & Test.output "solutions" solutions

overlappingSegments ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Text (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok (Just (Curve2d.OverlappingSegments segments)) -> Ok segments
    Ok (Just (Curve2d.IntersectionPoints _)) ->
      Error "Should have found some overlapping segments, got intersection points instead"
    Ok Nothing -> Error "Should have found some overlapping segments"
    Error error -> Error (Text.show error)

equalUBounds :: Bounds Unitless -> Bounds Unitless -> Bool
equalUBounds (Bounds actualLow actualHigh) (Bounds expectedLow expectedHigh) =
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
  let arc1 = Curve2d.arc (Point2d.meters 1 0) (Point2d.meters -1 0) Angle.halfTurn
  let arc2 = Curve2d.arc (Point2d.meters 0 -1) (Point2d.meters 0 1) Angle.halfTurn
  actualSegments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.one (OverlappingSegment (Bounds 0 0.5) (Bounds 0.5 1) Positive)
  Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "curveOverlap2" Test.do
  let arc1 =
        Curve2d.polarArc
          (#centerPoint Point2d.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle (negative Angle.pi))
  let arc2 =
        Curve2d.polarArc
          (#centerPoint Point2d.origin)
          (#radius Length.meter)
          (#startAngle (Angle.degrees -45))
          (#endAngle (Angle.degrees 225))
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.two
          (OverlappingSegment (Bounds 0 (1 / 4)) (Bounds 0 (1 / 6)) Negative)
          (OverlappingSegment (Bounds (3 / 4) 1) (Bounds (5 / 6) 1) Negative)
  Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "crossingIntersection" Test.do
  let arc1 = Curve2d.arc Point2d.origin (Point2d.meters 0 1) Angle.halfTurn
  let arc2 = Curve2d.arc Point2d.origin (Point2d.meters 1 0) (negative Angle.halfTurn)
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.two
          (IntersectionPoint.crossing 0 0 Positive)
          (IntersectionPoint.crossing 0.5 0.5 Negative)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 (Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints))
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "tangentIntersection" Test.do
  let arc1 =
        Curve2d.polarArc
          (#centerPoint Point2d.origin)
          (#radius Length.meter)
          (#startAngle Angle.zero)
          (#endAngle Angle.pi)
  let arc2 =
        Curve2d.polarArc
          (#centerPoint (Point2d.meters 0 1.5))
          (#radius (Length.meters 0.5))
          (#startAngle (negative Angle.pi))
          (#endAngle Angle.zero)
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersectionPoints = NonEmpty.one (IntersectionPoint.tangent 0.5 0.5 Positive)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve2d.IntersectionPoints actualIntersectionPoints) ->
      Tolerance.using 1e-12 do
        Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints)
          & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
          & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just (Curve2d.OverlappingSegments _) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

solving :: Tolerance Meters => Test
solving = Test.verify "solving" Test.do
  let arc = Curve2d.arc (Point2d.meters 0 1) (Point2d.meters 1 0) Angle.quarterTurn
  let distanceFromOrigin = VectorCurve2d.magnitude (arc .-. Point2d.origin)
  let desiredDistance = Length.meters 0.5
  zeros <- Curve.zeros (distanceFromOrigin .-. desiredDistance)
  let distanceAt zero = Point2d.distanceFrom Point2d.origin (Curve2d.evaluate arc zero.location)
  Test.expect (List.map distanceAt zeros ~= [desiredDistance, desiredDistance])

degenerateStartPointTangent :: Tolerance Meters => Test
degenerateStartPointTangent = Test.check 100 "degenerateStartPointTangent" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = Curve2d.cubicBezier p0 p0 p1 p2
  let decreasingTValues = [Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let startTangent = DirectionCurve2d.startValue tangentDirection
  let otherTangents = List.map (DirectionCurve2d.evaluate tangentDirection) decreasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2d.angleFrom startTangent otherTangent)
  let angleDifferences = List.map angleDifference otherTangents
  Test.expect (List.isDescending angleDifferences)

degenerateEndPointTangent :: Tolerance Meters => Test
degenerateEndPointTangent = Test.check 100 "degenerateEndPointTangent" Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  let curve = Curve2d.cubicBezier p0 p1 p2 p2
  let increasingTValues = [1 -. Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
  tangentDirection <- Curve2d.tangentDirection curve
  let endTangent = DirectionCurve2d.endValue tangentDirection
  let otherTangents = List.map (DirectionCurve2d.evaluate tangentDirection) increasingTValues
  let angleDifference otherTangent = Quantity.abs (Direction2d.angleFrom endTangent otherTangent)
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
    Test.expect (Tolerance.using 1e-12 (derivative `dot` tangent ~= 0))
      & Test.output "tValue" tValue
      & Test.output "tangent" tangent
      & Test.output "derivative" derivative
      & Test.output "dot product" (derivative `dot` tangent)

degenerateStartPointTangentDerivative :: Tolerance Meters => Test
degenerateStartPointTangentDerivative =
  Test.check 100 "degenerateStartPointTangentDerivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = Curve2d.cubicBezier p0 p0 p1 p2
    let decreasingTValues = [Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let startTangentDerivative = VectorCurve2d.startValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2d.evaluate tangentDerivative) decreasingTValues
    let differences =
          List.map Vector2d.magnitude $
            List.map (.-. startTangentDerivative) otherTangentDerivatives
    Test.expect (List.isDescending differences)
      & Test.output "differences" differences
      & Test.output "startTangentDerivative" startTangentDerivative

degenerateEndPointTangentDerivative :: Tolerance Meters => Test
degenerateEndPointTangentDerivative =
  Test.check 100 "degenerateEndPointTangentDerivative" Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    let curve = Curve2d.cubicBezier p0 p1 p2 p2
    let increasingTValues = [1 -. Number.pow 2 (Number.fromInt -n) | n <- [8 :: Int .. 16]]
    tangentDirection <- Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    let endTangentDerivative = VectorCurve2d.endValue tangentDerivative
    let otherTangentDerivatives =
          List.map (VectorCurve2d.evaluate tangentDerivative) increasingTValues
    let differences =
          List.map Vector2d.magnitude $
            List.map (.-. endTangentDerivative) otherTangentDerivatives
    Test.expect (List.isDescending differences)
      & Test.output "differences" differences
      & Test.output "endTangentDerivative" endTangentDerivative

firstDerivativeIsConsistent :: Curve2d (space @ Meters) -> Number -> Expectation
firstDerivativeIsConsistent = firstDerivativeIsConsistentWithin (Length.meters 1e-6)

firstDerivativeIsConsistentWithin ::
  Show (Quantity units) =>
  Quantity units ->
  Curve2d (space @ units) ->
  Number ->
  Expectation
firstDerivativeIsConsistentWithin givenTolerance curve tValue = do
  let dt :: Number = 1e-6
  let p1 = Curve2d.evaluate curve (tValue .-. dt)
  let p2 = Curve2d.evaluate curve (tValue .+. dt)
  let numericalFirstDerivative = (p2 .-. p1) ./ (2 *. dt)
  let analyticFirstDerivative = VectorCurve2d.evaluate curve.derivative tValue
  Tolerance.using givenTolerance do
    Test.expect (numericalFirstDerivative ~= analyticFirstDerivative)
      & Test.output "numericalFirstDerivative" numericalFirstDerivative
      & Test.output "analyticFirstDerivative" analyticFirstDerivative

firstDerivativeConsistency :: Generator (Curve2d (space @ Meters)) -> Test
firstDerivativeConsistency curveGenerator = Test.check 100 "firstDerivativeConsistency" Test.do
  curve <- curveGenerator
  t <- Parameter.random
  firstDerivativeIsConsistent curve t

secondDerivativeIsConsistent :: Curve2d (space @ Meters) -> Number -> Expectation
secondDerivativeIsConsistent curve tValue = do
  let dt :: Number = 1e-6
  let v1 = VectorCurve2d.evaluate curve.derivative (tValue .-. dt)
  let v2 = VectorCurve2d.evaluate curve.derivative (tValue .+. dt)
  let numericalSecondDerivative = (v2 .-. v1) ./. (2 *. dt)
  let secondDerivative = curve.derivative.derivative
  let analyticSecondDerivative = VectorCurve2d.evaluate secondDerivative tValue
  Tolerance.using (Length.meters 1e-6) do
    Test.expect (numericalSecondDerivative ~= analyticSecondDerivative)
      & Test.output "numericalSecondDerivative" numericalSecondDerivative
      & Test.output "analyticSecondDerivative" analyticSecondDerivative

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
          Test.expect (Curve2d.evaluate curve t ~= Curve2d.evaluate reversedCurve (1 -. t))

boundsConsistency ::
  (Tolerance units, Show (Quantity units)) =>
  Curve2d (space @ units) ->
  Expectation
boundsConsistency curve = Test.do
  tBounds <- Bounds.random Parameter.random
  tValue <- Random.map (Bounds.interpolate tBounds) Parameter.random
  let curveValue = Curve2d.evaluate curve tValue
  let curveBounds = Curve2d.evaluateBounds curve tBounds
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
        let expectedPoint = Point2d.meters expectedX expectedY
        Test.verify label Test.do
          let arc = Curve2d.arc Point2d.origin (Point2d.meters 1 1) sweptAngle
          Test.expect (Curve2d.evaluate arc 0.5 ~= expectedPoint)
  let invSqrt2 = 1 /. Number.sqrt 2
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1 -. invSqrt2)
    , testArcMidpoint -90 (1 -. invSqrt2, invSqrt2)
    , testArcMidpoint 180 (1, 0)
    , testArcMidpoint -180 (0, 1)
    ]

arcDeformation :: Tolerance Meters => Test
arcDeformation = Test.check 100 "deformation" Test.do
  initialArc <- Random.arc2d
  transform <- Random.affineTransform2d
  t <- Parameter.random
  let transformedArc = Curve2d.transformBy transform initialArc
  let pointOnTransformed = Curve2d.evaluate transformedArc t
  let transformOfStart = Point2d.transformBy transform initialArc.startPoint
  let transformOfEnd = Point2d.transformBy transform initialArc.endPoint
  let transformOfPoint = Point2d.transformBy transform (Curve2d.evaluate initialArc t)
  Test.all
    [ Test.expect (transformedArc.startPoint ~= transformOfStart)
        & Test.output "transformedArc.startPoint" transformedArc.startPoint
        & Test.output "transformOfStart" transformOfStart
    , Test.expect (transformedArc.endPoint ~= transformOfEnd)
    , Test.expect (pointOnTransformed ~= transformOfPoint)
    ]

g2 :: Tolerance Meters => Test
g2 = Test.check 100 "G2 continuity" Test.do
  p1 <- Random.point2d
  p2 <- Random.point2d
  p3 <- Random.point2d
  p4 <- Random.point2d
  let spline = Curve2d.cubicBezier p1 p2 p3 p4
  t <- Parameter.random
  let point = Curve2d.evaluate spline t
  tangentCurve <- Curve2d.tangentDirection spline
  curvatureCurve <- Curve2d.curvature spline
  let tangentDirection = DirectionCurve2d.evaluate tangentCurve t
  let signedRadius = 1 /. Curve.evaluate curvatureCurve t
  let normalDirection = Direction2d.rotateLeft tangentDirection
  let arcCenter = point .+. signedRadius .*. normalDirection
  let arc = Curve2d.sweptArc arcCenter point (Angle.degrees 30)
  Test.expect (Curve2d.g2 (spline, t) (arc, 0) Length.meter)
