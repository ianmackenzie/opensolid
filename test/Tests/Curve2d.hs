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
import Expect (Expectation)
import Expect qualified
import Length qualified
import List qualified
import OpenSolid
import Point2d qualified
import Qty qualified
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

equalParameterValues :: List Float -> List Float -> Expectation
equalParameterValues actual expected =
  Expect.list Expect.approximately expected actual
 where
  ?tolerance = 1e-9

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
  Expect.all
    [ equalParameterValues startParameterValues [0.0]
    , equalParameterValues endParameterValues [1.0]
    , equalParameterValues midParameterValues [0.5]
    , equalParameterValues offCurveParameterValues []
    ]

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result Text (List (Domain, Domain))
overlappingSegments curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Ok _ -> Error "Intersection should have failed (and given overlapping segments)"
    Error (Curve2d.CurvesOverlap segments) -> Ok segments
    Error error -> Error (errorMessage error)

expectSegments :: List (Domain, Domain) -> List (Domain, Domain) -> Expectation
expectSegments expected actual =
  Expect.list (Expect.pair Expect.range) expected actual
 where
  ?tolerance = 1e-12

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "Overlap detection 1" $ do
  arc1 <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 1.0 0.0)
      , Arc2d.EndPoint (Point2d.meters -1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 0.0 -1.0)
      , Arc2d.EndPoint (Point2d.meters 0.0 1.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  segments <- overlappingSegments arc1 arc2
  segments |> expectSegments [(Range.from 0.0 0.5, Range.from 0.5 1.0)]

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
  segments
    |> expectSegments
      [ (Range.from 0.0 (1 / 4), Range.from 0.0 (1 / 6))
      , (Range.from (3 / 4) 1.0, Range.from (5 / 6) 1.0)
      ]

expectIntersection :: Intersection -> Intersection -> Expectation
expectIntersection expected actual
  | expected.kind /= actual.kind = Expect.fail "Intersection kinds do not match"
  | expected.sign /= actual.sign = Expect.fail "Intersection signs do not match"
  | expected.u1 != actual.u1 || expected.u2 != actual.u2 =
      Expect.fail "Intersection parameter values do not match"
  | otherwise = Expect.pass
 where
  ?tolerance = 1e-12

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "Crossing intersection" $ do
  arc1 <-
    Arc2d.with
      [ Arc2d.StartPoint Point2d.origin
      , Arc2d.EndPoint (Point2d.meters 0.0 1.0)
      , Arc2d.SweptAngle (Angle.degrees 180.0)
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.StartPoint Point2d.origin
      , Arc2d.EndPoint (Point2d.meters 1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees -180.0)
      ]
  intersections <- Curve2d.intersections arc1 arc2
  intersections
    |> Expect.list
      expectIntersection
      [ Intersection 0.0 0.0 Intersection.Crossing Positive
      , Intersection 0.5 0.5 Intersection.Crossing Negative
      ]

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "Tangent intersection" $ do
  arc1 <-
    Arc2d.with
      [ Arc2d.CenterPoint Point2d.origin
      , Arc2d.Radius Length.meter
      , Arc2d.StartAngle Qty.zero
      , Arc2d.EndAngle Angle.halfTurn
      ]
  arc2 <-
    Arc2d.with
      [ Arc2d.CenterPoint (Point2d.meters 0.0 1.5)
      , Arc2d.Radius (Length.meters 0.5)
      , Arc2d.StartAngle -Angle.halfTurn
      , Arc2d.EndAngle Qty.zero
      ]
  intersections <- Curve2d.intersections arc1 arc2
  intersections
    |> Expect.list
      expectIntersection
      [Intersection 0.5 0.5 Intersection.Tangent Positive]

solving :: Tolerance Meters => Test
solving = Test.verify "Solving via Curve1d" $ do
  arc <-
    Arc2d.with
      [ Arc2d.StartPoint (Point2d.meters 0.0 1.0)
      , Arc2d.EndPoint (Point2d.meters 1.0 0.0)
      , Arc2d.SweptAngle (Angle.degrees 90.0)
      ]
  let squaredDistanceFromOrigin = VectorCurve2d.squaredMagnitude (arc - Point2d.origin)
  let desiredDistance = Length.meters 0.5
  roots <- squaredDistanceFromOrigin |> Curve1d.equalToSquared (Length.meters 0.5)
  let distances =
        roots
          |> List.map Curve1d.Root.value
          |> List.map (Curve2d.pointOn arc)
          |> List.map (Point2d.distanceFrom Point2d.origin)
  distances |> Expect.list Expect.approximately [desiredDistance, desiredDistance]