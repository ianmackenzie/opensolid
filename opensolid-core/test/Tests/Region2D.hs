module Tests.Region2D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length qualified as Length
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity (zero)
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Region2D.BoundedBy qualified as Region2D.BoundedBy
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

areaIsApproximately :: Area -> Region2D Meters space -> Bool
areaIsApproximately expectedArea region = do
  let measuredArea = Estimate.within (Area.squareMeters 1e-4) (Region2D.area region)
  Interval.includes expectedArea measuredArea

square :: Tolerance Meters => Test
square = Test.verify "square" Test.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  let line1 = Curve2D.lineFrom p1 p2
  let line2 = Curve2D.lineFrom p2 p3
  let line3 = Curve2D.lineFrom p4 p3
  let line4 = Curve2D.lineFrom p4 p1
  region <- Region2D.boundedBy [line1, line3, line2, line4]
  Test.expect (areaIsApproximately (width * width) region)

quarterCircle :: Tolerance Meters => Test
quarterCircle = Test.verify "quarterCircle" Test.do
  let radius = Length.meters 1.0
  let p1 = Point2D.origin
  let p2 = Point2D radius zero
  let p3 = Point2D zero radius
  let line1 = Curve2D.lineFrom p1 p2
  let line2 = Curve2D.lineFrom p1 p3
  let arc = Curve2D.arcFrom p2 p3 Angle.quarterTurn
  region <- Region2D.boundedBy [line1, line2, arc]
  let expectedArea = 0.25 * Number.pi * radius * radius
  Test.expect (areaIsApproximately expectedArea region)

squareWithHole :: Tolerance Meters => Test
squareWithHole = Test.verify "squareWithHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  let line1 = Curve2D.lineFrom p1 p2
  let line2 = Curve2D.lineFrom p2 p3
  let line3 = Curve2D.lineFrom p4 p3
  let line4 = Curve2D.lineFrom p4 p1
  let centerPoint = Point2D (0.5 * width) (0.5 * width)
  let holeDiameter = 0.5 * width
  let holeRadius = 0.5 * holeDiameter
  let hole = Curve2D.circle (Circle2D.withDiameter holeDiameter centerPoint)
  region <- Region2D.boundedBy [line1, line3, line2, line4, hole]
  let expectedArea = width * width - Number.pi * holeRadius * holeRadius
  Test.expect (areaIsApproximately expectedArea region)

incompleteSquare :: Tolerance Meters => Test
incompleteSquare = Test.verify "incompleteSquare" Test.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  let line1 = Curve2D.lineFrom p1 p2
  let line2 = Curve2D.lineFrom p2 p3
  let line3 = Curve2D.lineFrom p4 p3
  case Region2D.boundedBy [line1, line2, line3] of
    Ok _ -> Test.fail "Expected region construction to fail on incomplete boundary"
    Error error -> Test.expect (error == Region2D.BoundedBy.BoundaryHasGaps)

squareWithTangentHole :: Tolerance Meters => Test
squareWithTangentHole = Test.verify "squareWithTangentHole" Test.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  let line1 = Curve2D.lineFrom p1 p2
  let line2 = Curve2D.lineFrom p2 p3
  let line3 = Curve2D.lineFrom p4 p3
  let line4 = Curve2D.lineFrom p4 p1
  let centerPoint = Point2D (0.5 * width) (0.5 * width)
  let hole = Curve2D.circle (Circle2D.withDiameter width centerPoint)
  case Region2D.boundedBy [line1, line2, line3, line4, hole] of
    Ok _ -> Test.fail "Expected non-manifold region construction to fail"
    Error error -> Test.expect (error == Region2D.BoundedBy.BoundaryIntersectsItself)

twoCircles :: Tolerance Meters => Test
twoCircles = Test.verify "twoCircles" Test.do
  let circle1 = Curve2D.circle (Circle2D.withDiameter (Length.meters 2.0) (Point2D.meters -2.0 0.0))
  let circle2 = Curve2D.circle (Circle2D.withDiameter (Length.meters 1.0) (Point2D.meters 1.0 0.0))
  case Region2D.boundedBy [circle1, circle2] of
    Ok _ -> Test.fail "Expected region construction to fail when given two disjoint circles"
    Error error -> Test.expect (error == Region2D.BoundedBy.MultipleDisjointRegions)
