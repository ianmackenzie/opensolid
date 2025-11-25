module Tests.Bounds3d (tests) where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified
import Tests.Random qualified

tests :: Tolerance Meters => List Test
tests =
  [ placeIn
  , relativeTo
  , transformBy
  , projectInto
  , distanceAlong
  ]

boundsAndContainedPoint :: Generator (Bounds3d space, Point3d space)
boundsAndContainedPoint = do
  bounds <- Tests.Random.bounds3d
  u <- Parameter.random
  v <- Parameter.random
  w <- Parameter.random
  let point = Bounds3d.interpolate bounds u v w
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localPoint) <- boundsAndContainedPoint
  frame <- Tests.Random.frame3d
  let globalBounds = Bounds3d.placeIn frame localBounds
  let globalPoint = Point3d.placeIn frame localPoint
  Test.expect (globalPoint `intersects` globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalPoint) <- boundsAndContainedPoint
  frame <- Tests.Random.frame3d
  let localBounds = Bounds3d.relativeTo frame globalBounds
  let localPoint = Point3d.relativeTo frame globalPoint
  Test.expect (localPoint `intersects` localBounds)

projectInto :: Tolerance Meters => Test
projectInto = Test.check 100 "projectInto" Test.do
  (bounds3d, point3d) <- boundsAndContainedPoint
  plane <- Tests.Random.plane3d
  let bounds2d = Bounds3d.projectInto plane bounds3d
  let point2d = Point3d.projectInto plane point3d
  Test.expect (point2d `intersects` bounds2d)

distanceAlong :: Tolerance Meters => Test
distanceAlong = Test.check 100 "distanceAlong" Test.do
  (bounds3d, point3d) <- boundsAndContainedPoint
  axis <- Tests.Random.axis3d
  let distanceBounds = Bounds3d.distanceAlong axis bounds3d
  let distance = Point3d.distanceAlong axis point3d
  Test.expect (distance `intersects` distanceBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalPoint) <- boundsAndContainedPoint
  transform <- Tests.Random.affineTransform3d
  let transformedBounds = Bounds3d.transformBy transform originalBounds
  let transformedPoint = Point3d.transformBy transform originalPoint
  Test.expect (transformedPoint `intersects` transformedBounds)
