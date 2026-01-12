module Tests.Bounds3D (tests) where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
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

boundsAndContainedPoint :: Generator (Bounds3D space, Point3D space)
boundsAndContainedPoint = do
  bounds <- Tests.Random.bounds3D
  u <- Parameter.random
  v <- Parameter.random
  w <- Parameter.random
  let point = Bounds3D.interpolate bounds u v w
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localPoint) <- boundsAndContainedPoint
  frame <- Tests.Random.frame3D
  let globalBounds = Bounds3D.placeIn frame localBounds
  let globalPoint = Point3D.placeIn frame localPoint
  Test.expect (globalPoint `intersects` globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalPoint) <- boundsAndContainedPoint
  frame <- Tests.Random.frame3D
  let localBounds = Bounds3D.relativeTo frame globalBounds
  let localPoint = Point3D.relativeTo frame globalPoint
  Test.expect (localPoint `intersects` localBounds)

projectInto :: Tolerance Meters => Test
projectInto = Test.check 100 "projectInto" Test.do
  (bounds3D, point3D) <- boundsAndContainedPoint
  plane <- Tests.Random.plane3D
  let bounds2D = Bounds3D.projectInto plane bounds3D
  let point2D = Point3D.projectInto plane point3D
  Test.expect (point2D `intersects` bounds2D)

distanceAlong :: Tolerance Meters => Test
distanceAlong = Test.check 100 "distanceAlong" Test.do
  (bounds3D, point3D) <- boundsAndContainedPoint
  axis <- Tests.Random.axis3D
  let distanceBounds = Bounds3D.distanceAlong axis bounds3D
  let distance = Point3D.distanceAlong axis point3D
  Test.expect (distance `intersects` distanceBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalPoint) <- boundsAndContainedPoint
  transform <- Tests.Random.affineTransform3D
  let transformedBounds = Bounds3D.transformBy transform originalBounds
  let transformedPoint = Point3D.transformBy transform originalPoint
  Test.expect (transformedPoint `intersects` transformedBounds)
