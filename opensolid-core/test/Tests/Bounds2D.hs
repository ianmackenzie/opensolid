module Tests.Bounds2D (tests) where

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ placeIn
  , relativeTo
  , transformBy
  ]

boundsAndContainedPoint :: Generator (Bounds2D Meters, Point2D Meters)
boundsAndContainedPoint = do
  bounds <- Random.bounds2D
  u <- Parameter.random
  v <- Parameter.random
  let point = Bounds2D.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Test
placeIn = Test.check 100 "placeIn" do
  (localBounds, localPoint) <- Test.generate boundsAndContainedPoint
  frame <- Test.generate Random.frame2D
  let globalBounds = Bounds2D.placeIn frame localBounds
  let globalPoint = Point2D.placeIn frame localPoint
  Test.expect (globalPoint `intersects` globalBounds)

relativeTo :: Test
relativeTo = Test.check 100 "relativeTo" do
  (globalBounds, globalPoint) <- Test.generate boundsAndContainedPoint
  frame <- Test.generate Random.frame2D
  let localBounds = Bounds2D.relativeTo frame globalBounds
  let localPoint = Point2D.relativeTo frame globalPoint
  Test.expect (localPoint `intersects` localBounds)

transformBy :: Test
transformBy = Test.check 100 "transformBy" do
  (originalBounds, originalPoint) <- Test.generate boundsAndContainedPoint
  transform <- Test.generate Random.affineTransform2D
  let transformedBounds = Bounds2D.transformBy transform originalBounds
  let transformedPoint = Point2D.transformBy transform originalPoint
  Test.expect (transformedPoint `intersects` transformedBounds)
