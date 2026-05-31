module Tests.Set2D (tests) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Text qualified as Text
import OpenSolid.UvBounds (pattern UvBounds)
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ findAll
  , clusters
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

equalSets :: Eq item => Set2D units item -> Set2D space item -> Bool
equalSets set1 set2 = Set2D.toList set1 == Set2D.toList set2

clusters :: Test
clusters = Test.verify "clusters" do
  let a = UvBounds (Interval 0.1 0.2) (Interval 0.2 0.3)
  let b = UvBounds (Interval 0.1 0.2) (Interval 0.1 0.2)
  let c = UvBounds (Interval 0.2 0.3) (Interval 0.1 0.2)
  let d = UvBounds (Interval 0.2 0.3) (Interval 0.4 0.5)
  let e = UvBounds (Interval 0.3 0.4) (Interval 0.3 0.4)
  let f = UvBounds (Interval 0.5 0.6) (Interval 0.1 0.2)
  let items = NonEmpty.six a b c d e f
  let set = Set2D.build id items
  let boundsPredicate bounds1 bounds2 = Bounds2D.overlap bounds1 bounds2 >= 0.0
  let itemPredicate _ _ = True
  case NonEmpty.sortBy Set2D.size (Set2D.clusters boundsPredicate itemPredicate set) of
    NonEmpty.Three first second third ->
      Test.all
        [ Test.expect (equalSets first (Set2D.build id (NonEmpty.one f)))
        , Test.expect (equalSets second (Set2D.build id (NonEmpty.two d e)))
        , Test.expect (equalSets third (Set2D.build id (NonEmpty.three a b c)))
        ]
    unexpected ->
      Test.fail $ "Expected three clusters, got " <> Text.int (NonEmpty.length unexpected)
