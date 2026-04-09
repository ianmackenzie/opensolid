module Tests.VectorBounds3D (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.Volume qualified as Volume
import Test (Test)
import Test qualified
import Tests.Random qualified

tests :: List Test
tests =
  [ magnitude
  , placeIn
  , relativeTo
  , transformBy
  , tripleProduct
  ]

magnitude :: Test
magnitude = Test.check 100 "magnitude" do
  vectorBounds <- Test.generate Tests.Random.vectorBounds3D
  tx <- Test.generate Parameter.random
  ty <- Test.generate Parameter.random
  tz <- Test.generate Parameter.random
  let vector = VectorBounds3D.interpolate vectorBounds tx ty tz
  let vectorMagnitude = Vector3D.magnitude vector
  let magnitudeBounds = VectorBounds3D.magnitude vectorBounds
  Test.expect (vectorMagnitude `intersects` magnitudeBounds)

boundsAndContainedVector :: Generator (VectorBounds3D Meters space, Vector3D Meters space)
boundsAndContainedVector = do
  bounds <- Tests.Random.vectorBounds3D
  u <- Parameter.random
  v <- Parameter.random
  w <- Parameter.random
  let vector = VectorBounds3D.interpolate bounds u v w
  Random.return (bounds, vector)

placeIn :: Test
placeIn = Test.check 100 "placeIn" do
  (localBounds, localVector) <- Test.generate boundsAndContainedVector
  frame <- Test.generate Tests.Random.frame3D
  let globalBounds = VectorBounds3D.placeIn frame localBounds
  let globalVector = Vector3D.placeIn frame localVector
  Test.expect (globalVector `intersects` globalBounds)

relativeTo :: Test
relativeTo = Test.check 100 "relativeTo" do
  (globalBounds, globalVector) <- Test.generate boundsAndContainedVector
  frame <- Test.generate Tests.Random.frame3D
  let localBounds = VectorBounds3D.relativeTo frame globalBounds
  let localVector = Vector3D.relativeTo frame globalVector
  Test.expect (localVector `intersects` localBounds)

transformBy :: Test
transformBy = Test.check 100 "transformBy" do
  (originalBounds, originalVector) <- Test.generate boundsAndContainedVector
  transform <- Test.generate Tests.Random.affineTransform3D
  let transformedBounds = VectorBounds3D.transformBy transform originalBounds
  let transformedVector = Vector3D.transformBy transform originalVector
  Test.expect (transformedVector `intersects` transformedBounds)

tripleProduct :: Test
tripleProduct = Test.check 1000 "tripleProduct" do
  (bounds1, vector1) <- Test.generate boundsAndContainedVector
  (bounds2, vector2) <- Test.generate boundsAndContainedVector
  (bounds3, vector3) <- Test.generate boundsAndContainedVector
  let boundsTripleProduct = VectorBounds3D.tripleProduct bounds1 bounds2 bounds3
  let vectorTripleProduct = (vector1 `cross` vector2) `dot` vector3
  Tolerance.using (Tolerance.unitless * Volume.cubicMeter) $
    Test.expect (vectorTripleProduct `intersects` boundsTripleProduct)
