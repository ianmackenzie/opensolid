module Tests.Region2d (tests) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Float qualified
import Length qualified
import Line2d qualified
import OpenSolid
import Point2d qualified
import Qty (zero)
import Region2d qualified
import Test (Test)
import Test qualified
import Tolerance qualified
import Units (Meters)

tests :: Tolerance Meters => List Test
tests =
  [ square
  , quarterCircle
  , squareWithHole
  , incompleteSquare
  , squareWithTangentHole
  , twoCircles
  ]

square :: Tolerance Meters => Test
square = Test.verify "square" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p2 p3
  line3 <- Line2d.from p4 p3
  line4 <- Line2d.from p4 p1
  region <- Region2d.boundedBy [line1, line3, line2, line4]
  Test.expect (Tolerance.using (Area.squareMeters 1e-6) (Region2d.area region ~= width * width))

quarterCircle :: Tolerance Meters => Test
quarterCircle = Test.verify "quarterCircle" Test.do
  let radius = Length.meters 1.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy radius zero
  let p3 = Point2d.xy zero radius
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p1 p3
  arc <- Arc2d.from p2 p3 Angle.quarterTurn
  region <- Region2d.boundedBy [line1, line2, arc]
  Test.expect (Tolerance.using (Area.squareMeters 1e-6) (Region2d.area region ~= 0.25 * Float.pi * radius * radius))

squareWithHole :: Tolerance Meters => Test
squareWithHole = Test.verify "squareWithHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p2 p3
  line3 <- Line2d.from p4 p3
  line4 <- Line2d.from p4 p1
  let centerPoint = Point2d.xy (width / 2.0) (width / 2.0)
  let holeRadius = width / 4.0
  arc <-
    Arc2d.with
      ( Arc2d.centerPoint centerPoint
      , Arc2d.radius holeRadius
      , Arc2d.startAngle zero
      , Arc2d.sweptAngle (Angle.degrees 360.0)
      )
  region <- Region2d.boundedBy [line1, line3, line2, line4, arc]
  let area = Region2d.area region
  let expectedArea = width * width - Float.pi * holeRadius * holeRadius
  Test.expect (Tolerance.using (Area.squareMeters 1e-6) (area ~= expectedArea))

incompleteSquare :: Tolerance Meters => Test
incompleteSquare = Test.verify "incompleteSquare" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p2 p3
  line3 <- Line2d.from p4 p3
  case Region2d.boundedBy [line1, line2, line3] of
    Ok _ -> Test.fail "Expected region construction to fail on incomplete boundary"
    Error error -> Test.expect (error == Region2d.RegionBoundaryHasGaps)

squareWithTangentHole :: Tolerance Meters => Test
squareWithTangentHole = Test.verify "squareWithTangentHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p2 p3
  line3 <- Line2d.from p4 p3
  line4 <- Line2d.from p4 p1
  let centerPoint = Point2d.xy (width / 2.0) (width / 2.0)
  let holeRadius = width / 2.0
  arc <-
    Arc2d.with
      ( Arc2d.centerPoint centerPoint
      , Arc2d.radius holeRadius
      , Arc2d.startAngle zero
      , Arc2d.sweptAngle (Angle.degrees 360.0)
      )
  case Region2d.boundedBy [line1, line2, line3, line4, arc] of
    Ok _ -> Test.fail "Expected non-manifold region construction to fail"
    Error error -> Test.expect (error == Region2d.RegionBoundaryIntersectsItself)

twoCircles :: Tolerance Meters => Test
twoCircles = Test.verify "twoCircles" Test.do
  let circle centerPoint radius =
        Arc2d.with
          ( Arc2d.centerPoint centerPoint
          , Arc2d.radius radius
          , Arc2d.startAngle zero
          , Arc2d.sweptAngle Angle.fullTurn
          )
  circle1 <- circle (Point2d.meters -2.0 0.0) (Length.meters 1.0)
  circle2 <- circle (Point2d.meters 1.0 0.0) (Length.meters 0.5)
  case Region2d.boundedBy [circle1, circle2] of
    Ok _ -> Test.fail "Expected region construction to fail when given two disjoint circles"
    Error error -> Test.expect (error == Region2d.MultipleDisjointRegions)
