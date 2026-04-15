module Tests.Set2D (tests) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ findAll
  ]

point :: Int -> Int -> Point2D Meters
point x y = Point2D.centimeters (Number.fromInt x) (Number.fromInt y)

testSet :: Set2D Meters (Point2D Meters)
testSet =
  Set2D.build Bounds2D.constant $
    NonEmpty.eight
      (point 1 1)
      (point 3 5)
      (point 7 2)
      (point 3 8)
      (point 8 3)
      (point 5 5)
      (point 2 9)
      (point 7 3)

findAll :: Test
findAll = Test.verify "findAll" do
  let searchBounds = Bounds2D.hull2 (point 2 2) (point 6 6)
  let foundPoints = Set2D.cull (intersects searchBounds) testSet
  let expectedPoints = [point 3 5, point 5 5]
  Test.expect (List.sort foundPoints == List.sort expectedPoints)
