module Tests.Set2D (tests) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
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

tests :: Tolerance Meters => List Test
tests =
  [ find
  , filter
  ]

point :: Int -> Int -> Point2D Meters space
point x y = Point2D.centimeters (Number.fromInt x) (Number.fromInt y)

testSet :: Set2D Meters space (Point2D Meters space)
testSet =
  Set2D.fromNonEmpty $
    NonEmpty.eight
      (point 1 1)
      (point 3 5)
      (point 7 2)
      (point 3 8)
      (point 8 3)
      (point 5 5)
      (point 2 9)
      (point 7 3)

find :: Tolerance Meters => Test
find =
  Test.group "find" $
    [ Test.verify "point" Test.do
        let searchBounds = Bounds2D.hull2 (point 0 0) (point 2 2)
        Test.expect (Set2D.find searchBounds testSet == Resolved (Just (point 1 1)))
    , Test.verify "nothing" Test.do
        let searchBounds = Bounds2D.hull2 (point 4 9) (point 5 10)
        Test.expect (Set2D.find searchBounds testSet == Resolved Nothing)
    , Test.verify "unresolved" Test.do
        let searchBounds = Bounds2D.hull2 (point 3 3) (point 6 6)
        Test.expect (Set2D.find searchBounds testSet == Unresolved)
    ]

filter :: Tolerance Meters => Test
filter = Test.verify "filter" Test.do
  let searchBounds = Bounds2D.hull2 (point 2 2) (point 6 6)
  let filteredPoints = Set2D.filter searchBounds testSet
  let expectedPoints = [point 3 5, point 5 5]
  Test.expect (List.sort filteredPoints == List.sort expectedPoints)
