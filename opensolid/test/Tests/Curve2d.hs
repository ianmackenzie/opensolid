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
import DirectionCurve2d qualified
import Error qualified
import Length qualified
import List qualified
import OpenSolid
import Parameter qualified
import Point2d qualified
import Qty qualified
import QuadraticSpline2d qualified
import Range (Range (Range))
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Tolerance qualified
import Units (Meters)
import VectorCurve2d qualified

tests :: Tolerance Meters => List Test
tests =
  [ find
  , curveOverlap1
  , curveOverlap2
  , crossingIntersection
  , tangentIntersection
  , solving
  , tangentDerivativeIsPerpendicularToTangent
  ]

find :: Tolerance Meters => Test
find =
  Test.verify "find" <| Test.do
    let p1 = Point2d.meters 0.0 0.0
    let p2 = Point2d.meters 1.0 2.0
    let p3 = Point2d.meters 2.0 0.0
    testSpline <- QuadraticSpline2d.fromControlPoints p1 p2 p3
    let startParameterValues = Curve2d.find Point2d.origin testSpline
    let endParameterValues = Curve2d.find (Point2d.meters 2.0 0.0) testSpline
    let midParameterValues = Curve2d.find (Point2d.meters 1.0 1.0) testSpline
    let offCurveParameterValues = Curve2d.find (Point2d.meters 1.0 1.1) testSpline
    let ?tolerance = 1e-12
     in Test.expectAll
          [ startParameterValues ~= [0.0]
          , endParameterValues ~= [1.0]
          , midParameterValues ~= [0.5]
          , offCurveParameterValues == []
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
equalUBounds (Range low1 high1) (Range low2 high2) =
  let ?tolerance = 1e-12 in low1 ~= low2 && high1 ~= high2

equalOverlapSegments :: (Range Unitless, Range Unitless, Sign) -> (Range Unitless, Range Unitless, Sign) -> Bool
equalOverlapSegments (t1, t2, sign) (t1', t2', sign') =
  equalUBounds t1 t1' && equalUBounds t2 t2' && sign == sign'

equalOverlapSegmentLists :: List (Range Unitless, Range Unitless, Sign) -> List (Range Unitless, Range Unitless, Sign) -> Bool
equalOverlapSegmentLists segments1 segments2 =
  List.allTrue (List.map2 equalOverlapSegments segments1 segments2)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 =
  Test.verify "Overlap detection 1" <| Test.do
    arc1 <- Arc2d.swept Angle.halfTurn (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0)
    arc2 <- Arc2d.swept Angle.halfTurn (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0)
    segments <- overlappingSegments arc1 arc2
    let expectedSegments = [(Range.from 0.0 0.5, Range.from 0.5 1.0, Positive)]
     in Test.expect (equalOverlapSegmentLists segments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 =
  Test.verify "Overlap detection 2" <| Test.do
    arc1 <-
      Arc2d.with
        ( Arc2d.centerPoint Point2d.origin
        , Arc2d.radius (Length.meters 1.0)
        , Arc2d.startAngle (Angle.degrees 0.0)
        , Arc2d.endAngle (Angle.degrees -180.0)
        )
    arc2 <-
      Arc2d.with
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
     in Test.expect (equalOverlapSegmentLists segments expectedSegments)

crossingIntersection :: Tolerance Meters => Test
crossingIntersection =
  Test.verify "Crossing intersection" <| Test.do
    arc1 <- Arc2d.swept Angle.halfTurn Point2d.origin (Point2d.meters 0.0 1.0)
    arc2 <- Arc2d.swept -Angle.halfTurn Point2d.origin (Point2d.meters 1.0 0.0)
    intersections <- Curve2d.intersections arc1 arc2
    let expectedIntersections =
          [ Intersection 0.0 0.0 Intersection.Crossing Positive
          , Intersection 0.5 0.5 Intersection.Crossing Negative
          ]
    let ?tolerance = 1e-12
     in Test.expect (intersections ~= expectedIntersections)

tangentIntersection :: Tolerance Meters => Test
tangentIntersection =
  Test.verify "Tangent intersection" <| Test.do
    arc1 <-
      Arc2d.with
        ( Arc2d.centerPoint Point2d.origin
        , Arc2d.radius Length.meter
        , Arc2d.startAngle (Angle.degrees 0.0)
        , Arc2d.endAngle (Angle.degrees 180.0)
        )
    arc2 <-
      Arc2d.with
        ( Arc2d.centerPoint (Point2d.meters 0.0 1.5)
        , Arc2d.radius (Length.meters 0.5)
        , Arc2d.startAngle (Angle.degrees -180.0)
        , Arc2d.endAngle (Angle.degrees 0.0)
        )
    intersections <- Curve2d.intersections arc1 arc2
    let expectedIntersections = [Intersection 0.5 0.5 Intersection.Tangent Positive]
    let ?tolerance = 1e-12
     in Test.expect (intersections ~= expectedIntersections)

solving :: Tolerance Meters => Test
solving =
  Test.verify "Solving via Curve1d" <| Test.do
    arc <- Arc2d.swept Angle.quarterTurn (Point2d.meters 0.0 1.0) (Point2d.meters 1.0 0.0)
    let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
    let desiredDistance = Length.meters 0.5
    roots <-
      let ?tolerance = Tolerance.ofSquared desiredDistance
       in Curve1d.zeros (squaredDistanceFromOrigin - Qty.squared desiredDistance)
    let distances =
          roots
            |> List.map Curve1d.Root.value
            |> List.map (Curve2d.pointOn arc)
            |> List.map (Point2d.distanceFrom Point2d.origin)
    Test.expect (distances ~= [desiredDistance, desiredDistance])

tangentDerivativeIsPerpendicularToTangent :: Tolerance Meters => Test
tangentDerivativeIsPerpendicularToTangent =
  Test.check 100 "Tangent derivative is perpendicular to tangent" <| Test.do
    p0 <- Random.point2d
    p1 <- Random.point2d
    p2 <- Random.point2d
    p3 <- Random.point2d
    curve <- CubicSpline2d.fromControlPoints p0 p1 p2 p3
    let tangentDirection = Curve2d.tangentDirection curve
    let tangentDerivative = DirectionCurve2d.derivative tangentDirection
    t <- Parameter.generator
    let tangent = DirectionCurve2d.evaluateAt t tangentDirection
    let derivative = VectorCurve2d.evaluateAt t tangentDerivative
    Test.expect (let ?tolerance = 1e-12 in derivative <> tangent ~= 0.0)
      |> Test.output "t" t
      |> Test.output "tangent" tangent
      |> Test.output "derivative" derivative
      |> Test.output "dot product" (derivative <> tangent)
