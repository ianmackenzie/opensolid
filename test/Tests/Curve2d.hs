module Tests.Curve2d (tests) where

import Angle qualified
import Arc2d qualified
import Curve1d qualified
import Curve1d.Root qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Domain (Domain)
import Length qualified
import List qualified
import OpenSolid
import Point2d qualified
import QuadraticSpline2d qualified
import Range qualified
import Test (Test)
import Test qualified
import Units (Meters)
import VectorCurve2d qualified

tests :: Tolerance Meters => List Test
tests =
  [ parameterValues
  , curveOverlap1
  , curveOverlap2
  , crossingIntersection
  , tangentIntersection
  , solving
  ]

parameterValues :: Tolerance Meters => Test
parameterValues = Test.verify "parameterValues" $ do
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
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Text (List (Domain, Domain, Sign))
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok _ -> Error "Intersection should have failed (and given overlapping segments)"
    Error (Curve2d.CurvesOverlap segments) -> Ok segments
    Error error -> Error (errorMessage error)

equalOverlapSegments :: (Domain, Domain, Sign) -> (Domain, Domain, Sign) -> Bool
equalOverlapSegments (u1, v1, sign1) (u2, v2, sign2) =
  let ?tolerance = 1e-12
   in u1 ~= u2 && v1 ~= v2 && sign1 == sign2

equalOverlapSegmentLists :: List (Domain, Domain, Sign) -> List (Domain, Domain, Sign) -> Bool
equalOverlapSegmentLists segments1 segments2 =
  List.allTrue (List.map2 equalOverlapSegments segments1 segments2)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "Overlap detection 1" $ do
  arc1 <- Arc2d.from (Point2d.meters 1.0 0.0) (Point2d.meters -1.0 0.0) Angle.halfTurn
  arc2 <- Arc2d.from (Point2d.meters 0.0 -1.0) (Point2d.meters 0.0 1.0) Angle.halfTurn
  segments <- overlappingSegments arc1 arc2
  let expectedSegments = [(Range.from 0.0 0.5, Range.from 0.5 1.0, Positive)]
   in Test.expect (equalOverlapSegmentLists segments expectedSegments)

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "Overlap detection 2" $ do
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

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "Crossing intersection" $ do
  arc1 <- Arc2d.from Point2d.origin (Point2d.meters 0.0 1.0) Angle.halfTurn
  arc2 <- Arc2d.from Point2d.origin (Point2d.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Curve2d.intersections arc1 arc2
  let expectedIntersections =
        [ Intersection 0.0 0.0 Intersection.Crossing Positive
        , Intersection 0.5 0.5 Intersection.Crossing Negative
        ]
  let ?tolerance = 1e-12
   in Test.expect (intersections ~= expectedIntersections)

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "Tangent intersection" $ do
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

solving :: Tolerance Meters => Test
solving = Test.verify "Solving via Curve1d" $ do
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
