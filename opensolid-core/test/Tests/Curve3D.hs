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
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D
import Test (Test)
import Test qualified

tests :: Tolerance Meters => List Test
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
    Ok (Just (Curve.OverlappingSegments sign segments)) -> Ok (sign, segments)
    Ok (Just (Curve.IntersectionPoints _)) ->
      Error "Should have found some overlapping segments, got intersection points instead"
    Ok Nothing -> Error "Should have found some overlapping segments"
    Error error -> Error (Text.show error)

equalParameterBounds :: Interval Unitless -> Interval Unitless -> Bool
equalParameterBounds (Interval actualLow actualHigh) (Interval expectedLow expectedHigh) =
  Tolerance.using 1e-12 (actualLow ~= expectedLow && actualHigh ~= expectedHigh)

equalOverlapSegments ::
  ((Interval Unitless, Interval Unitless), (Interval Unitless, Interval Unitless)) ->
  Bool
equalOverlapSegments ((actual1, actual2), (expected1, expected2)) =
  equalParameterBounds actual1 expected1 && equalParameterBounds actual2 expected2

equalOverlapSegmentLists ::
  NonEmpty (Interval Unitless, Interval Unitless) ->
  NonEmpty (Interval Unitless, Interval Unitless) ->
  Bool
equalOverlapSegmentLists actualSegments expectedSegments =
  NonEmpty.length actualSegments == NonEmpty.length expectedSegments
    && NonEmpty.all equalOverlapSegments (NonEmpty.zip2 actualSegments expectedSegments)

curveOverlap1 :: Tolerance Meters => Test
curveOverlap1 = Test.verify "curveOverlap1" do
  let arc1 =
        Curve3D.on World3D.topPlane $
          Curve2D.arcFrom (Point2D.meters 1.0 0.0) (Point2D.meters -1.0 0.0) Angle.halfTurn
  let arc2 =
        Curve3D.on World3D.topPlane $
          Curve2D.arcFrom (Point2D.meters 0.0 -1.0) (Point2D.meters 0.0 1.0) Angle.halfTurn
  (sign, actualSegments) <- Result.orFail (overlappingSegments arc1 arc2)
  let expectedSegments = NonEmpty.one (Interval 0.0 0.5, Interval 0.5 1.0)
  Test.all
    [ Test.expect (equalOverlapSegmentLists actualSegments expectedSegments)
    , Test.expect (sign == Positive)
    ]

curveOverlap2 :: Tolerance Meters => Test
curveOverlap2 = Test.verify "curveOverlap2" do
  let arc1 =
        Curve3D.on World3D.topPlane $
          Curve2D.polarArc
            (#centerPoint Point2D.origin)
            (#radius Length.meter)
            (#startAngle Angle.zero)
            (#endAngle -Angle.pi)
  let arc2 =
        Curve3D.on World3D.topPlane $
          Curve2D.polarArc
            (#centerPoint Point2D.origin)
            (#radius Length.meter)
            (#startAngle (Angle.degrees -45.0))
            (#endAngle (Angle.degrees 225.0))
  (sign, segments) <- Result.orFail (overlappingSegments arc1 arc2)
  let expectedSegments =
        NonEmpty.two
          (Interval 0.0 (1 / 4), Interval 0.0 (1 / 6))
          (Interval (3 / 4) 1.0, Interval (5 / 6) 1.0)
  Test.all
    [ Test.expect (equalOverlapSegmentLists segments expectedSegments)
    , Test.expect (sign == Negative)
    ]

crossingIntersection :: Tolerance Meters => Test
crossingIntersection = Test.verify "crossingIntersection" do
  let arc1 =
        Curve3D.on World3D.topPlane $
          Curve2D.arcFrom Point2D.origin (Point2D.meters 0.0 1.0) Angle.halfTurn
  let arc2 =
        Curve3D.on World3D.topPlane $
          Curve2D.arcFrom Point2D.origin (Point2D.meters 1.0 0.0) -Angle.halfTurn
  intersections <- Result.orFail (Curve3D.intersections arc1 arc2)
  let expectedIntersectionPoints =
        NonEmpty.two (IntersectionPoint.crossing 0.0 0.0) (IntersectionPoint.crossing 0.5 0.5)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints)
        & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
        & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just Curve.OverlappingSegments{} ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"

tangentIntersection :: Tolerance Meters => Test
tangentIntersection = Test.verify "tangentIntersection" do
  let arc1 =
        Curve3D.on World3D.topPlane $
          Curve2D.polarArc
            (#centerPoint Point2D.origin)
            (#radius Length.meter)
            (#startAngle Angle.zero)
            (#endAngle Angle.pi)
  let arc2 =
        Curve3D.on World3D.topPlane $
          Curve2D.polarArc
            (#centerPoint (Point2D.meters 0.0 1.5))
            (#radius (Length.meters 0.5))
            (#startAngle -Angle.pi)
            (#endAngle Angle.zero)
  intersections <- Result.orFail (Curve3D.intersections arc1 arc2)
  let expectedIntersectionPoints = NonEmpty.one (IntersectionPoint.tangent 0.5 0.5 Negative)
  case intersections of
    Nothing -> Test.fail "Should have found some intersection points"
    Just (Curve.IntersectionPoints actualIntersectionPoints) ->
      Test.expect (actualIntersectionPoints ~= expectedIntersectionPoints)
        & Test.output "expectedIntersectionPoints" expectedIntersectionPoints
        & Test.output "actualIntersectionPoints" actualIntersectionPoints
    Just (Curve.OverlappingSegments{}) ->
      Test.fail "Should have found some intersection points, got overlapping segments instead"
