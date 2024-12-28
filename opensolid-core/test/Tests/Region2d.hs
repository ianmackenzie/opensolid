module Tests.Region2d (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2d qualified as Arc2d
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Float qualified as Float
import OpenSolid.Length qualified as Length
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty (zero)
import OpenSolid.Range qualified as Range
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Region2d.BoundedBy qualified as Region2d.BoundedBy
import OpenSolid.Units (Meters)
import Test (Test)
import Test qualified

tests :: Tolerance Meters => List Test
tests =
  [ square
  , quarterCircle
  , squareWithHole
  , incompleteSquare
  , squareWithTangentHole
  , twoCircles
  ]

areaIsApproximately :: Area -> Region2d (space @ Meters) -> Bool
areaIsApproximately expectedArea region =
  Estimate.within (Area.squareMeters 1e-4) (Region2d.area region)
    |> Range.includes expectedArea

square :: Tolerance Meters => Test
square = Test.verify "square" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  let line1 = Line2d.from p1 p2
  let line2 = Line2d.from p2 p3
  let line3 = Line2d.from p4 p3
  let line4 = Line2d.from p4 p1
  region <- Region2d.boundedBy [line1, line3, line2, line4]
  Test.expect (areaIsApproximately (width * width) region)

quarterCircle :: Tolerance Meters => Test
quarterCircle = Test.verify "quarterCircle" Test.do
  let radius = Length.meters 1.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy radius zero
  let p3 = Point2d.xy zero radius
  let line1 = Line2d.from p1 p2
  let line2 = Line2d.from p1 p3
  let arc = Arc2d.from p2 p3 Angle.quarterTurn
  region <- Region2d.boundedBy [line1, line2, arc]
  let expectedArea = 0.25 * Float.pi * radius * radius
  Test.expect (areaIsApproximately expectedArea region)

squareWithHole :: Tolerance Meters => Test
squareWithHole = Test.verify "squareWithHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  let line1 = Line2d.from p1 p2
  let line2 = Line2d.from p2 p3
  let line3 = Line2d.from p4 p3
  let line4 = Line2d.from p4 p1
  let centerPoint = Point2d.xy (0.5 * width) (0.5 * width)
  let holeRadius = 0.25 * width
  let hole = Arc2d.circle centerPoint holeRadius
  region <- Region2d.boundedBy [line1, line3, line2, line4, hole]
  let expectedArea = width * width - Float.pi * holeRadius * holeRadius
  Test.expect (areaIsApproximately expectedArea region)

incompleteSquare :: Tolerance Meters => Test
incompleteSquare = Test.verify "incompleteSquare" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  let line1 = Line2d.from p1 p2
  let line2 = Line2d.from p2 p3
  let line3 = Line2d.from p4 p3
  case Region2d.boundedBy [line1, line2, line3] of
    Success _ -> Test.fail "Expected region construction to fail on incomplete boundary"
    Failure error -> Test.expect (error == Region2d.BoundedBy.BoundaryHasGaps)

squareWithTangentHole :: Tolerance Meters => Test
squareWithTangentHole = Test.verify "squareWithTangentHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy width zero
  let p3 = Point2d.xy width width
  let p4 = Point2d.xy zero width
  let line1 = Line2d.from p1 p2
  let line2 = Line2d.from p2 p3
  let line3 = Line2d.from p4 p3
  let line4 = Line2d.from p4 p1
  let centerPoint = Point2d.xy (0.5 * width) (0.5 * width)
  let holeRadius = 0.5 * width
  let hole = Arc2d.circle centerPoint holeRadius
  case Region2d.boundedBy [line1, line2, line3, line4, hole] of
    Success _ -> Test.fail "Expected non-manifold region construction to fail"
    Failure error -> Test.expect (error == Region2d.BoundedBy.BoundaryIntersectsItself)

twoCircles :: Tolerance Meters => Test
twoCircles = Test.verify "twoCircles" Test.do
  let circle1 = Arc2d.circle (Point2d.meters -2.0 0.0) (Length.meters 1.0)
  let circle2 = Arc2d.circle (Point2d.meters 1.0 0.0) (Length.meters 0.5)
  case Region2d.boundedBy [circle1, circle2] of
    Success _ -> Test.fail "Expected region construction to fail when given two disjoint circles"
    Failure error -> Test.expect (error == Region2d.BoundedBy.MultipleDisjointRegions)