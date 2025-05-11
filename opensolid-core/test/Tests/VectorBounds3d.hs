module Tests.VectorBounds3d (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Units (Meters)
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
boundsAndContainedVector = Random.do
  bounds <- Tests.Random.vectorBounds3d
  u <- Parameter.random
  v <- Parameter.random
  w <- Parameter.random
  let vector = VectorBounds3d.interpolate bounds u v w
  Random.return (bounds, vector)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localVector) <- boundsAndContainedVector
  basis <- Tests.Random.basis3d
  let globalBounds = VectorBounds3d.placeIn basis localBounds
  let globalVector = Vector3d.placeIn basis localVector
  Test.expect (globalVector ^ globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalVector) <- boundsAndContainedVector
  basis <- Tests.Random.basis3d
  let localBounds = VectorBounds3d.relativeTo basis globalBounds
  let localVector = Vector3d.relativeTo basis globalVector
  Test.expect (localVector ^ localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalVector) <- boundsAndContainedVector
  transform <- Tests.Random.affineTransform3d
  let transformedBounds = VectorBounds3d.transformBy transform originalBounds
  let transformedVector = Vector3d.transformBy transform originalVector
  Test.expect (transformedVector ^ transformedBounds)
