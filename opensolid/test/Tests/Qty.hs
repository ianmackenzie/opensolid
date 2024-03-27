module Tests.Qty (tests) where

import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Random (Generator)
import Random qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Units (Meters)

tests :: List Test
tests =
  [ smallestBy
  , largestBy
  ]

pointListGenerator :: Generator (NonEmpty (Point2d (space @ Meters)))
pointListGenerator = Random.nonEmpty 10 Random.point2d

xMagnitude :: Point2d (space @ units) -> Qty units
xMagnitude point = Qty.abs (Point2d.xCoordinate point)

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" Test.do
  points <- pointListGenerator
  let smallest = Qty.smallestBy Point2d.xCoordinate points
  Test.expect (NonEmpty.all (\point -> xMagnitude point >= xMagnitude smallest) points)

largestBy :: Test
largestBy = Test.check 100 "largestBy" Test.do
  points <- pointListGenerator
  let largest = Qty.largestBy Point2d.xCoordinate points
  Test.expect (NonEmpty.all (\point -> xMagnitude point <= xMagnitude largest) points)
