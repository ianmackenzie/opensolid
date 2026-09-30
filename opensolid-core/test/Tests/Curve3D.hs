module Tests.Curve3D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Try qualified as Try
import OpenSolid.World3D qualified as World3D
import Test (Test, (~~))
import Test qualified

tests :: List Test
tests =
  [ curveOverlap1
  , curveOverlap2
  , crossingIntersection
  , tangentIntersection
  ]

overlappingSegments ::
  Tolerance Meters =>
  Curve3D space ->
  Curve3D space ->
  Result Text (Sign, NonEmpty (Interval Unitless, Interval Unitless))
overlappingSegments curve1 curve2 =
  case Curve3D.intersections curve1 curve2 of
    Just (Curve.OverlappingSegments sign segments _) -> Ok (sign, segments)
    Just (Curve.IntersectionPoints {}) ->
      Err "Should have found some overlapping segments, got intersection points instead"
    Nothing -> Err "Should have found some overlapping segments"

curveOverlap1 :: Test
curveOverlap1 = Test.verify "curveOverlap1" Try.do
  arc1 <-
    on World3D.topPlane do
      Curve2D.arcFrom (Point2D.meters 1.0 0.0) (Point2D.meters -1.0 0.0) Angle.halfTurn
  arc2 <-
    on World3D.topPlane do
      Curve2D.arcFrom (Point2D.meters 0.0 -1.0) (Point2D.meters 0.0 1.0) Angle.halfTurn
  (sign, actualSegments) <- overlappingSegments arc1 arc2
  let expectedSegments = NonEmpty.one (Interval 0.0 0.5, Interval 0.5 1.0)
  Test.all
    [ Test.expect (actualSegments ~~ expectedSegments)
    , Test.expect (sign == Positive)
    ]

curveOverlap2 :: Test
curveOverlap2 = Test.verify "curveOverlap2" Try.do
  arc1 <-
    on World3D.topPlane do
      Curve2D.polarArc
        { centerPoint = Point2D.origin
        , radius = Length.meter
        , startAngle = Angle.zero
        , endAngle = -Angle.pi
        }
  arc2 <-
    on World3D.topPlane do
      Curve2D.polarArc
        { centerPoint = Point2D.origin
        , radius = Length.meter
        , startAngle = Angle.degrees -45.0
        , endAngle = Angle.degrees 225.0
        }
  (sign, segments) <- overlappingSegments arc1 arc2
  let expectedSegments =
        NonEmpty.two
          (Interval 0.0 (1 / 4), Interval 0.0 (1 / 6))
          (Interval (3 / 4) 1.0, Interval (5 / 6) 1.0)
  Test.all
    [ Test.expect (segments ~~ expectedSegments)
    , Test.expect (sign == Negative)
    ]

crossingIntersection :: Test
crossingIntersection = Test.verify "crossingIntersection" Try.do
  arc1 <-
    on World3D.topPlane do
      Curve2D.arcFrom Point2D.origin (Point2D.meters 0.0 1.0) Angle.halfTurn
  arc2 <-
    on World3D.topPlane do
      Curve2D.arcFrom Point2D.origin (Point2D.meters 1.0 0.0) -Angle.halfTurn
  let intersections = Curve3D.intersections arc1 arc2
  let expectedIntersectionPoints =
        NonEmpty.two
          (IntersectionPoint.crossing (0.0, 0.0))
          (IntersectionPoint.crossing (0.5, 0.5))
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (actualIntersectionPoints ~~ expectedIntersectionPoints)
        & Test.log {expectedIntersectionPoints, actualIntersectionPoints}
    Just (Curve.OverlappingSegments {}) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Test
tangentIntersection = Test.verify "tangentIntersection" Try.do
  arc1 <-
    on World3D.topPlane do
      Curve2D.polarArc
        { centerPoint = Point2D.origin
        , radius = Length.meter
        , startAngle = Angle.zero
        , endAngle = Angle.pi
        }
  arc2 <-
    on World3D.topPlane do
      Curve2D.polarArc
        { centerPoint = Point2D.meters 0.0 1.5
        , radius = Length.meters 0.5
        , startAngle = -Angle.pi
        , endAngle = Angle.zero
        }
  let intersections = Curve3D.intersections arc1 arc2
  let expectedIntersectionPoints = NonEmpty.one (IntersectionPoint.tangent Negative (0.5, 0.5))
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (actualIntersectionPoints ~~ expectedIntersectionPoints)
        & Test.log {expectedIntersectionPoints, actualIntersectionPoints}
    Just (Curve.OverlappingSegments {}) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"
