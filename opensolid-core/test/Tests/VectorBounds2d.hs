module Tests.VectorBounds2d (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
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

boundsAndContainedVector :: Generator (VectorBounds2d (space @ Meters), Vector2d (space @ Meters))
boundsAndContainedVector = do
  bounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let point = VectorBounds2d.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localVector) <- boundsAndContainedVector
  frame <- Random.frame2d
  let globalBounds = VectorBounds2d.placeIn frame localBounds
  let globalVector = Vector2d.placeIn frame localVector
  Test.expect (globalVector ^ globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalVector) <- boundsAndContainedVector
  frame <- Random.frame2d
  let localBounds = VectorBounds2d.relativeTo frame globalBounds
  let localVector = Vector2d.relativeTo frame globalVector
  Test.expect (localVector ^ localBounds)

placeOn :: Tolerance Meters => Test
placeOn = Test.check 100 "placeOn" Test.do
  (bounds2d, vector2d) <- boundsAndContainedVector
  plane <- Random.plane3d
  let bounds3d = VectorBounds2d.placeOn plane bounds2d
  let vector3d = Vector2d.placeOn plane vector2d
  Test.expect (vector3d ^ bounds3d)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalVector) <- boundsAndContainedVector
  transform <- Random.affineTransform2d
  let transformedBounds = VectorBounds2d.transformBy transform originalBounds
  let transformedVector = Vector2d.transformBy transform originalVector
  Test.expect (transformedVector ^ transformedBounds)
