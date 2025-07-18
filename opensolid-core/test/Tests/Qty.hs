module Tests.Qty (tests) where

import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random (Generator)
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ smallestBy
  , largestBy
  ]

pointListGenerator :: Generator (NonEmpty (Point2d (space @ Meters)))
pointListGenerator = NonEmpty.random 10 Random.point2d

xMagnitude :: Point2d (space @ units) -> Qty units
xMagnitude point = Qty.abs (Point2d.xCoordinate point)

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" Test.do
  points <- pointListGenerator
  let smallest = Qty.smallestBy Point2d.xCoordinate points
  Test.expect (NonEmpty.allSatisfy (\point -> xMagnitude point >= xMagnitude smallest) points)

largestBy :: Test
largestBy = Test.check 100 "largestBy" Test.do
  points <- pointListGenerator
  let largest = Qty.largestBy Point2d.xCoordinate points
  Test.expect (NonEmpty.allSatisfy (\point -> xMagnitude point <= xMagnitude largest) points)
