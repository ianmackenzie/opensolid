module Tests.Region2d (tests) where

import Angle qualified
import Arc2d qualified
import Area qualified
import Float qualified
import Generic (zero)
import Length qualified
import Line2d qualified
import OpenSolid
import Point2d qualified
import Region2d qualified
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ square
  , quarterCircle
  , squareWithHole
  ]

square :: Test
square = Test.check 1 "square" $ do
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
  Test.expect (let ?tolerance = Area.squareMeters 1e-6 in Region2d.area region ~= width * width)
 where
  ?tolerance = Length.meters 1e-9

quarterCircle :: Test
quarterCircle = Test.check 1 "quarterCircle" $ do
  let radius = Length.meters 1.0
  let p1 = Point2d.origin
  let p2 = Point2d.xy radius zero
  let p3 = Point2d.xy zero radius
  line1 <- Line2d.from p1 p2
  line2 <- Line2d.from p1 p3
  arc <- Arc2d.with [Arc2d.StartPoint p2, Arc2d.EndPoint p3, Arc2d.SweptAngle (Angle.degrees 90.0)]
  region <- Region2d.boundedBy [line1, line2, arc]
  Test.expect (let ?tolerance = Area.squareMeters 1e-6 in Region2d.area region ~= 0.25 * Float.pi * radius * radius)
 where
  ?tolerance = Length.meters 1e-9

squareWithHole :: Test
squareWithHole = Test.check 1 "squareWithHole" $ do
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
      [ Arc2d.CenterPoint centerPoint
      , Arc2d.Radius holeRadius
      , Arc2d.StartAngle zero
      , Arc2d.SweptAngle (Angle.degrees 360.0)
      ]
  region <- Region2d.boundedBy [line1, line3, line2, line4, arc]
  let area = Region2d.area region
  let expectedArea = width * width - Float.pi * holeRadius * holeRadius
  Test.expect (let ?tolerance = Area.squareMeters 1e-6 in area ~= expectedArea)
 where
  ?tolerance = Length.meters 1e-9
