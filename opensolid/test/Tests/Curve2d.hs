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
import Float qualified
import Length qualified
import List qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Range (Range (Range))
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import U qualified
import Units (Meters)
import Vector2d qualified
import VectorCurve2d qualified

tests :: (Tolerance Meters) => List Test
tests =
  [ parameterValues
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
  ]

parameterValues :: (Tolerance Meters) => Test
parameterValues = Test.verify "parameterValues" $ Test.do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  testSpline <- QuadraticSpline2d.fromControlPoints p1 p2 p3
  let startParameterValues = Curve2d.parameterValues Point2d.origin testSpline
  let endParameterValues = Curve2d.parameterValues (Point2d.meters 2.0 0.0) testSpline
  let midParameterValues = Curve2d.parameterValues (Point2d.meters 1.0 1.0) testSpline
  let offCurveParameterValues = Curve2d.parameterValues (Point2d.meters 1.0 1.1) testSpline
  let ?tolerance = 1e-12
   in Test.expectAll
        [ startParameterValues ~= [0.0]
        , endParameterValues ~= [1.0]
        , midParameterValues ~= [0.5]
        , offCurveParameterValues == []
        ]

overlappingSegments ::
  (Tolerance units) =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result String (List (U.Bounds, U.Bounds, Sign))
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok _ -> Error "Intersection should have failed (and given overlapping segments)"
    Error (Curve2d.CurvesOverlap segments) -> Ok segments
    Error error -> Error (errorMessage error)

equalUBounds :: U.Bounds -> U.Bounds -> Bool
equalUBounds (Range u1 u2) (Range v1 v2) =
  let ?tolerance = 1e-12 in u1 ~= v1 && u2 ~= v2

equalOverlapSegments :: (U.Bounds, U.Bounds, Sign) -> (U.Bounds, U.Bounds, Sign) -> Bool
equalOverlapSegments (u1, v1, sign1) (u2, v2, sign2) =
  equalUBounds u1 u2 && equalUBounds v1 v2 && sign1 == sign2

equalOverlapSegmentLists :: List (U.Bounds, U.Bounds, Sign) -> List (U.Bounds, U.Bounds, Sign) -> Bool
equalOverlapSegmentLists segments1 segments2 =
  List.allTrue (List.map2 equalOverlapSegments segments1 segments2)

curveOverlap1 :: (Tolerance Meters) => Test
curveOverlap1 = Test.verify "Overlap detection 1" $ Test.do
  arc1 <- Arc2d.from (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) Angle.halfTurn
  arc2 <- Arc2d.from (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) Angle.halfTurn
  segments <- overlappingSegments arc1 arc2
  let expectedSegments = [(Range.from 0.0 0.5, Range.from 0.5 1.0, Positive)]
   in Test.expect (equalOverlapSegmentLists segments expectedSegments)

curveOverlap2 :: (Tolerance Meters) => Test
curveOverlap2 = Test.verify "Overlap detection 2" $ Test.do
  arc1 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius (Length.meters 1.0)
      , Arc2d.StartAngle (Angle.degrees 0.0)
      , Arc2d.EndAngle (Angle.degrees -180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius (Length.meters 1.0)
      , Arc2d.StartAngle (Angle.degrees -45.0)
      , Arc2d.EndAngle (Angle.degrees 225.0)
      ]
  segments <- overlappingSegments arc1 arc2
  let expectedSegments =
        [ (Range.from 0.0 (1 / 4), Range.from 0.0 (1 / 6), Negative)
        , (Range.from (3 / 4) 1.0, Range.from (5 / 6) 1.0, Negative)
        ]
   in Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: (Tolerance Meters) => Test
crossingIntersection = Test.verify "Crossing intersection" $ Test.do
  arc1 <- Arc2d.from Point2d.origin (Point2d.meters 0.0 1.0) Angle.halfTurn
  arc2 <- Arc2d.from Point2d.origin (Point2d.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersections =
        [ Intersection 0.0 0.0 Intersection.Crossing Positive
        , Intersection 0.5 0.5 Intersection.Crossing Negative
        ]
  let ?tolerance = 1e-12
   in Test.expect (intersections ~= expectedIntersections)

tangentIntersection :: (Tolerance Meters) => Test
tangentIntersection = Test.verify "Tangent intersection" $ Test.do
  arc1 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius Length.meter
      , Arc2d.StartAngle (Angle.degrees 0.0)
      , Arc2d.EndAngle (Angle.degrees 180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.CenterPoint (Point2d.meters 0.0 1.5)
      , Arc2d.Radius (Length.meters 0.5)
      , Arc2d.StartAngle (Angle.degrees -180.0)
      , Arc2d.EndAngle (Angle.degrees 0.0)
      ]
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersections = [Intersection 0.5 0.5 Intersection.Tangent Positive]
  let ?tolerance = 1e-12
   in Test.expect (intersections ~= expectedIntersections)

solving :: (Tolerance Meters) => Test
solving = Test.verify "Solving via Curve1d" $ Test.do
  arc <- Arc2d.from (Point2d.meters 0.0 1.0) (Point2d.meters 1.0 0.0) Angle.quarterTurn
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  let desiredDistance = Length.meters 0.5
  roots <- squaredDistanceFromOrigin |> Curve1d.equalToSquared (Length.meters 0.5)
  let distances =
        roots
          |> List.map Curve1d.Root.value
          |> List.map (Curve2d.pointOn arc)
          |> List.map (Point2d.distanceFrom Point2d.origin)
  Test.expect (distances ~= [desiredDistance, desiredDistance])

degenerateStartPointTangent :: (Tolerance Meters) => Test
degenerateStartPointTangent = Test.check 100 "Degenerate start point" $ Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  curve <- CubicSpline2d.fromControlPoints p0 p0 p1 p2
  let decreasingParameterValues = List.map (\n -> Float.pow 2.0 (Float.fromInt -n)) [8 .. 16]
  let tangentDirection = Curve2d.tangentDirection curve
  let startTangent = DirectionCurve2d.evaluateAt 0.0 tangentDirection
  let otherTangents =
        List.map (\t -> DirectionCurve2d.evaluateAt t tangentDirection) decreasingParameterValues
  let differences =
        otherTangents
          |> List.map Direction2d.unwrap
          |> List.map (- Direction2d.unwrap startTangent)
          |> List.map Vector2d.magnitude
  Test.expect (List.successive (-) differences |> List.all (> Qty.zero))

degenerateEndPointTangent :: (Tolerance Meters) => Test
degenerateEndPointTangent = Test.check 100 "Degenerate end point" $ Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  curve <- CubicSpline2d.fromControlPoints p0 p1 p2 p2
  let increasingParameterValues = List.map (\n -> 1.0 - Float.pow 2.0 (Float.fromInt -n)) [8 .. 16]
  let tangentDirection = Curve2d.tangentDirection curve
  let endTangent = DirectionCurve2d.evaluateAt 1.0 tangentDirection
  let otherTangents =
        List.map (\t -> DirectionCurve2d.evaluateAt t tangentDirection) increasingParameterValues
  let differences =
        otherTangents
          |> List.map Direction2d.unwrap
          |> List.map (- Direction2d.unwrap endTangent)
          |> List.map Vector2d.magnitude
  Test.expect (List.successive (-) differences |> List.all (> Qty.zero))

tangentDerivativeIsPerpendicularToTangent :: (Tolerance Meters) => Test
tangentDerivativeIsPerpendicularToTangent =
  Test.check 100 "Tangent derivative is perpendicular to tangent" $ Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    p3 <- Random.point2d
    curve <- CubicSpline2d.fromControlPoints p0 p1 p2 p3
    let tangentDirection = Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    u <- U.generator
    let tangent = DirectionCurve2d.evaluateAt u tangentDirection
    let derivative = VectorCurve2d.evaluateAt u tangentDerivative
    Test.expect (let ?tolerance = 1e-9 in derivative <> tangent ~= Qty.zero)
      |> Test.output "u" u
      |> Test.output "tangent" tangent
      |> Test.output "derivative" derivative
      |> Test.output "dot product" (derivative <> tangent)

degenerateStartPointTangentDerivative :: (Tolerance Meters) => Test
degenerateStartPointTangentDerivative = Test.check 100 "Degenerate start point derivative" $ Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  curve <- CubicSpline2d.fromControlPoints p0 p0 p1 p2
  let decreasingParameterValues = List.map (\n -> Float.pow 2.0 (Float.fromInt -n)) [8 .. 16]
  let tangentDirection = Curve2d.tangentDirection curve
  let tangentDerivative = DirectionCurve2d.derivative tangentDirection
  let startTangentDerivative = VectorCurve2d.evaluateAt 0.0 tangentDerivative
  let otherTangentDerivatives =
        List.map (\t -> VectorCurve2d.evaluateAt t tangentDerivative) decreasingParameterValues
  let differences =
        otherTangentDerivatives
          |> List.map (- startTangentDerivative)
          |> List.map Vector2d.magnitude
  Test.expect (List.successive (-) differences |> List.all (> Qty.zero))

degenerateEndPointTangentDerivative :: (Tolerance Meters) => Test
degenerateEndPointTangentDerivative = Test.check 100 "Degenerate end point derivative" $ Test.do
  p0 <- Random.point2d
  p1 <- Random.point2d
  p2 <- Random.point2d
  curve <- CubicSpline2d.fromControlPoints p0 p1 p2 p2
  let increasingParameterValues = List.map (\n -> 1.0 - Float.pow 2.0 (Float.fromInt -n)) [8 .. 16]
  let tangentDirection = Curve2d.tangentDirection curve
  let tangentDerivative = DirectionCurve2d.derivative tangentDirection
  let endTangentDerivative = VectorCurve2d.evaluateAt 1.0 tangentDerivative
  let otherTangentDerivatives =
        List.map (\t -> VectorCurve2d.evaluateAt t tangentDerivative) increasingParameterValues
  let differences =
        otherTangentDerivatives
          |> List.map (- endTangentDerivative)
          |> List.map Vector2d.magnitude
  Test.expect (List.successive (-) differences |> List.all (> Qty.zero))
