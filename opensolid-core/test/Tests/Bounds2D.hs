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

tests :: Tolerance Meters => List Test
tests =
  [ placeIn
  , relativeTo
  , transformBy
  ]

boundsAndContainedPoint :: Generator (Bounds2D Meters space, Point2D Meters space)
boundsAndContainedPoint = do
  bounds <- Random.bounds2D
  u <- Parameter.random
  v <- Parameter.random
  let point = Bounds2D.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localPoint) <- boundsAndContainedPoint
  frame <- Random.frame2D
  let globalBounds = Bounds2D.placeIn frame localBounds
  let globalPoint = Point2D.placeIn frame localPoint
  Test.expect (globalPoint `intersects` globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalPoint) <- boundsAndContainedPoint
  frame <- Random.frame2D
  let localBounds = Bounds2D.relativeTo frame globalBounds
  let localPoint = Point2D.relativeTo frame globalPoint
  Test.expect (localPoint `intersects` localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalPoint) <- boundsAndContainedPoint
  transform <- Random.affineTransform2D
  let transformedBounds = Bounds2D.transformBy transform originalBounds
  let transformedPoint = Point2D.transformBy transform originalPoint
  Test.expect (transformedPoint `intersects` transformedBounds)
