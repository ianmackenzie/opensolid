module Tests.Set2d (tests) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Float qualified as Float
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.Units (Meters)
import Test (Test)
import Test qualified

tests :: Tolerance Meters => List Test
tests =
  [ find
  , filter
  ]

point :: Int -> Int -> Point2d (space @ Meters)
point x y = Point2d.centimeters (Float.int x) (Float.int y)

testSet :: Set2d (Point2d (space @ Meters)) (space @ Meters)
testSet =
  Set2d.fromNonEmpty $
    NonEmpty.eight
      @ point 1 1
      @ point 3 5
      @ point 7 2
      @ point 3 8
      @ point 8 3
      @ point 5 5
      @ point 2 9
      @ point 7 3

find :: Tolerance Meters => Test
find =
  Test.group "find" $
    [ Test.verify "point" Test.do
        let searchBounds = Bounds2d.hull2 (point 0 0) (point 2 2)
        Test.expect (Set2d.find searchBounds testSet == Resolved (Just (point 1 1)))
    , Test.verify "nothing" Test.do
        let searchBounds = Bounds2d.hull2 (point 4 9) (point 5 10)
        Test.expect (Set2d.find searchBounds testSet == Resolved Nothing)
    , Test.verify "unresolved" Test.do
        let searchBounds = Bounds2d.hull2 (point 3 3) (point 6 6)
        Test.expect (Set2d.find searchBounds testSet == Unresolved)
    ]

filter :: Tolerance Meters => Test
filter = Test.verify "filter" Test.do
  let searchBounds = Bounds2d.hull2 (point 2 2) (point 6 6)
  let expectedPoints = [point 3 5, point 5 5]
  Test.expect (List.sort (Set2d.filter searchBounds testSet) == expectedPoints)
