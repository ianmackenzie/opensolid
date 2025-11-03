module Tests.VectorBounds3d (tests) where

import OpenSolid.Length qualified as Length
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
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
  vectorBounds <- Tests.Random.vectorBounds3d
  tx <- Parameter.random
  ty <- Parameter.random
  tz <- Parameter.random
  let vector = VectorBounds3d.interpolate vectorBounds tx ty tz
  let vectorMagnitude = Vector3d.magnitude vector
  let magnitudeBounds = VectorBounds3d.magnitude vectorBounds
  Test.expect (vectorMagnitude ^ magnitudeBounds)

boundsAndContainedVector :: Generator (VectorBounds3d (space @ Meters), Vector3d (space @ Meters))
boundsAndContainedVector = do
  bounds <- Tests.Random.vectorBounds3d
  u <- Parameter.random
  v <- Parameter.random
  w <- Parameter.random
  let vector = VectorBounds3d.interpolate bounds u v w
  Random.return (bounds, vector)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localVector) <- boundsAndContainedVector
  frame <- Tests.Random.frame3d
  let globalBounds = VectorBounds3d.placeIn frame localBounds
  let globalVector = Vector3d.placeIn frame localVector
  Test.expect (globalVector ^ globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalVector) <- boundsAndContainedVector
  frame <- Tests.Random.frame3d
  let localBounds = VectorBounds3d.relativeTo frame globalBounds
  let localVector = Vector3d.relativeTo frame globalVector
  Test.expect (localVector ^ localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalVector) <- boundsAndContainedVector
  transform <- Tests.Random.affineTransform3d
  let transformedBounds = VectorBounds3d.transformBy transform originalBounds
  let transformedVector = Vector3d.transformBy transform originalVector
  Test.expect (transformedVector ^ transformedBounds)

tripleProduct :: Test
tripleProduct = Test.check 1000 "tripleProduct" Test.do
  (bounds1, vector1) <- boundsAndContainedVector
  (bounds2, vector2) <- boundsAndContainedVector
  (bounds3, vector3) <- boundsAndContainedVector
  let boundsTripleProduct = VectorBounds3d.tripleProduct bounds1 bounds2 bounds3
  let vectorTripleProduct = (vector1 `cross'` vector2) `dot'` vector3
  Tolerance.using (1e-9 * (Length.meter .*. Length.meter .*. Length.meter)) $
    Test.expect (vectorTripleProduct ^ boundsTripleProduct)
