module Tests.VectorBounds3D (tests) where

import OpenSolid.Length qualified as Length
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import Test (Test)
import Test qualified
import Tests.Random qualified

tests :: Tolerance Meters => List Test
tests =
  [ magnitude
  , placeIn
  , relativeTo
  , transformBy
  , tripleProduct
  ]

magnitude :: Tolerance Meters => Test
magnitude = Test.check 100 "magnitude" Test.do
  vectorBounds <- Tests.Random.vectorBounds3D
  tx <- Parameter.random
  ty <- Parameter.random
  tz <- Parameter.random
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

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localVector) <- boundsAndContainedVector
  frame <- Tests.Random.frame3D
  let globalBounds = VectorBounds3D.placeIn frame localBounds
  let globalVector = Vector3D.placeIn frame localVector
  Test.expect (globalVector `intersects` globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalVector) <- boundsAndContainedVector
  frame <- Tests.Random.frame3D
  let localBounds = VectorBounds3D.relativeTo frame globalBounds
  let localVector = Vector3D.relativeTo frame globalVector
  Test.expect (localVector `intersects` localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalVector) <- boundsAndContainedVector
  transform <- Tests.Random.affineTransform3D
  let transformedBounds = VectorBounds3D.transformBy transform originalBounds
  let transformedVector = Vector3D.transformBy transform originalVector
  Test.expect (transformedVector `intersects` transformedBounds)

tripleProduct :: Test
tripleProduct = Test.check 1000 "tripleProduct" Test.do
  (bounds1, vector1) <- boundsAndContainedVector
  (bounds2, vector2) <- boundsAndContainedVector
  (bounds3, vector3) <- boundsAndContainedVector
  let boundsTripleProduct = VectorBounds3D.tripleProduct bounds1 bounds2 bounds3
  let vectorTripleProduct = (vector1 `cross_` vector2) `dot_` vector3
  Tolerance.using (1e-9 * (Length.meter ?*? Length.meter ?*? Length.meter)) $
    Test.expect (vectorTripleProduct `intersects` boundsTripleProduct)
