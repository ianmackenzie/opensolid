module Tests.Region2D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (data Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity (zero)
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Result qualified as Result
import OpenSolid.Try qualified as Try
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ squareArea
  , quarterCircleArea
  , squareWithHoleArea
  , incompleteSquare
  , squareWithTangentHole
  , twoCircles
  , pointContainment
  ]

areaIsApproximately :: Area -> Region2D Meters -> Bool
areaIsApproximately expectedArea region = do
  let measuredArea = Estimate.within (Area.squareMeters 1e-4) (Region2D.area region)
  Interval.member expectedArea measuredArea

squareArea :: Test
squareArea = Test.verify "squareArea" Try.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  line1 <- Curve2D.lineFrom p1 p2
  line2 <- Curve2D.lineFrom p2 p3
  line3 <- Curve2D.lineFrom p4 p3
  line4 <- Curve2D.lineFrom p4 p1
  region <- Region2D.boundedBy [line1, line3, line2, line4]
  Test.expect (areaIsApproximately (width * width) region)

quarterCircleArea :: Test
quarterCircleArea = Test.verify "quarterCircleArea" Try.do
  let radius = Length.meters 1.0
  let p1 = Point2D.origin
  let p2 = Point2D radius zero
  let p3 = Point2D zero radius
  line1 <- Curve2D.lineFrom p1 p2
  line2 <- Curve2D.lineFrom p1 p3
  arc <- Curve2D.arcFrom p2 p3 Angle.quarterTurn
  region <- Region2D.boundedBy [line1, line2, arc]
  let expectedArea = 0.25 * Number.pi * radius * radius
  Test.expect (areaIsApproximately expectedArea region)

squareWithHoleArea :: Test
squareWithHoleArea = Test.verify "squareWithHoleArea" Try.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  line1 <- Curve2D.lineFrom p1 p2
  line2 <- Curve2D.lineFrom p2 p3
  line3 <- Curve2D.lineFrom p4 p3
  line4 <- Curve2D.lineFrom p4 p1
  let centerPoint = Point2D (0.5 * width) (0.5 * width)
  let holeDiameter = 0.5 * width
  let holeRadius = 0.5 * holeDiameter
  hole <- Curve2D.circleWith {centerPoint, diameter = holeDiameter}
  region <- Region2D.boundedBy [line1, line3, line2, line4, hole]
  let expectedArea = width * width - Number.pi * holeRadius * holeRadius
  Test.expect (areaIsApproximately expectedArea region)

incompleteSquare :: Test
incompleteSquare = Test.verify "incompleteSquare" Try.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  line1 <- Curve2D.lineFrom p1 p2
  line2 <- Curve2D.lineFrom p2 p3
  line3 <- Curve2D.lineFrom p4 p3
  case Region2D.boundedBy [line1, line2, line3] of
    Ok _ -> Test.fail "Expected region construction to fail on incomplete boundary"
    Err _ -> Test.pass

squareWithTangentHole :: Test
squareWithTangentHole = Test.verify "squareWithTangentHole" Try.do
  let width = Length.meters 2.0
  let p1 = Point2D.origin
  let p2 = Point2D width zero
  let p3 = Point2D width width
  let p4 = Point2D zero width
  line1 <- Curve2D.lineFrom p1 p2
  line2 <- Curve2D.lineFrom p2 p3
  line3 <- Curve2D.lineFrom p4 p3
  line4 <- Curve2D.lineFrom p4 p1
  let centerPoint = Point2D (0.5 * width) (0.5 * width)
  hole <- Curve2D.circleWith {centerPoint, diameter = width}
  case Region2D.boundedBy [line1, line2, line3, line4, hole] of
    Ok _ -> Test.fail "Expected non-manifold region construction to fail"
    Err _ -> Test.pass

twoCircles :: Test
twoCircles = Test.verify "twoCircles" Try.do
  let circle centerPoint diameter = Curve2D.circleWith {centerPoint, diameter}
  circle1 <- circle (Point2D.meters -2.0 0.0) (Length.meters 2.0)
  circle2 <- circle (Point2D.meters 1.0 0.0) (Length.meters 1.0)
  case Region2D.boundedBy [circle1, circle2] of
    Ok _ -> Test.fail "Expected region construction to fail when given two disjoint circles"
    Err _ -> Test.pass

pointContainment :: Test
pointContainment = Test.verify "pointContainment" Try.do
  let d = Length.centimeters 10.0
  let r = Length.centimeters 2.5
  let point i j = Point2D (i * d) (j * d)
  let circle i j = do
        curve <- Curve2D.circleWith {centerPoint = point i j, diameter = 2.0 * r}
        Ok [curve]
  let square i j = do
        let cx = i * d
        let cy = j * d
        let p1 = Point2D (cx - r) (cy - r)
        let p2 = Point2D (cx + r) (cy - r)
        let p3 = Point2D (cx + r) (cy + r)
        let p4 = Point2D (cx - r) (cy + r)
        line12 <- Curve2D.lineFrom p1 p2
        line23 <- Curve2D.lineFrom p2 p3
        line34 <- Curve2D.lineFrom p3 p4
        line41 <- Curve2D.lineFrom p4 p1
        Ok [line12, line23, line34, line41]
  let outerLoopPoints =
        [ point 0.0 0.0
        , point 3.0 0.0
        , point 3.0 2.0
        , point 2.0 2.0
        , point 2.0 1.0
        , point 1.0 1.0
        , point 1.0 2.0
        , point 0.0 2.0
        ]
  outerLoop <- Result.sequence (List.loop Curve2D.lineFrom outerLoopPoints)
  hole1 <- square 0.5 1.5
  hole2 <- circle 0.5 0.5
  hole3 <- square 1.5 0.5
  hole4 <- circle 2.5 0.5
  hole5 <- square 2.5 1.5
  let boundaryCurves = List.concat [outerLoop, hole1, hole2, hole3, hole4, hole5]
  region <- Region2D.boundedBy boundaryCurves
  let expectInside givenPoint = Test.expect (givenPoint ^ region)
  let expectOutside givenPoint = Test.expect (not (givenPoint ^ region))
  Test.all
    [ Test.combine expectInside outerLoopPoints
    , Test.combine expectInside $
        [ point 1.0 0.5
        , point 1.5 0.75
        , point 2.75 1.75
        ]
    , Test.combine expectOutside $
        [ point -0.5 0.5
        , point 0.5 0.5
        , point 0.5 1.5
        , point 1.25 1.25
        ]
    ]
