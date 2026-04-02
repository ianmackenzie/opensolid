module Tests.Quantity (tests) where

import OpenSolid.Length (Length)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ smallestBy
  , largestBy
  ]

randomPointList :: Generator (NonEmpty (Point2D Meters))
randomPointList = NonEmpty.random 10 Random.point2D

xMagnitude :: Point2D Meters -> Length
xMagnitude point = Quantity.abs (Point2D.xCoordinate point)

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" do
  points <- Test.generate randomPointList
  let smallest = NonEmpty.minimumBy (Quantity.abs . Point2D.xCoordinate) points
  Test.expect (NonEmpty.all (\point -> xMagnitude point >= xMagnitude smallest) points)

largestBy :: Test
largestBy = Test.check 100 "largestBy" do
  points <- Test.generate randomPointList
  let largest = NonEmpty.maximumBy (Quantity.abs . Point2D.xCoordinate) points
  Test.expect (NonEmpty.all (\point -> xMagnitude point <= xMagnitude largest) points)
