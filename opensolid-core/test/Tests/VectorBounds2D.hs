module Tests.VectorBounds2D (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: Tolerance Meters => List Test
tests =
  [ placeIn
  , relativeTo
  , placeOn
  , transformBy
  ]

boundsAndContainedVector :: Generator (VectorBounds2D Meters, Vector2D Meters)
boundsAndContainedVector = do
  bounds <- Random.vectorBounds2D
  u <- Parameter.random
  v <- Parameter.random
  let point = VectorBounds2D.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" do
  (localBounds, localVector) <- Test.generate boundsAndContainedVector
  frame <- Test.generate Random.frame2D
  let globalBounds = VectorBounds2D.placeIn frame localBounds
  let globalVector = Vector2D.placeIn frame localVector
  Test.expect (globalVector `intersects` globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" do
  (globalBounds, globalVector) <- Test.generate boundsAndContainedVector
  frame <- Test.generate Random.frame2D
  let localBounds = VectorBounds2D.relativeTo frame globalBounds
  let localVector = Vector2D.relativeTo frame globalVector
  Test.expect (localVector `intersects` localBounds)

placeOn :: Tolerance Meters => Test
placeOn = Test.check 100 "placeOn" do
  (bounds2D, vector2D) <- Test.generate boundsAndContainedVector
  plane <- Test.generate Random.plane3D
  let bounds3D = VectorBounds2D.placeOn plane bounds2D
  let vector3D = Vector2D.placeOn plane vector2D
  Test.expect (vector3D `intersects` bounds3D)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" do
  (originalBounds, originalVector) <- Test.generate boundsAndContainedVector
  transform <- Test.generate Random.affineTransform2D
  let transformedBounds = VectorBounds2D.transformBy transform originalBounds
  let transformedVector = Vector2D.transformBy transform originalVector
  Test.expect (transformedVector `intersects` transformedBounds)
