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
  , transformBy
  ]

boundsAndContainedVector :: Generator (VectorBounds2d (space @ Meters), Vector2d (space @ Meters))
boundsAndContainedVector = Random.do
  bounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let point = VectorBounds2d.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localVector) <- boundsAndContainedVector
  orientation <- Random.orientation2d
  let globalBounds = VectorBounds2d.placeIn orientation localBounds
  let globalVector = Vector2d.placeIn orientation localVector
  Test.expect (globalVector ^ globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalVector) <- boundsAndContainedVector
  orientation <- Random.orientation2d
  let localBounds = VectorBounds2d.relativeTo orientation globalBounds
  let localVector = Vector2d.relativeTo orientation globalVector
  Test.expect (localVector ^ localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalVector) <- boundsAndContainedVector
  transform <- Random.affineTransform2d
  let transformedBounds = VectorBounds2d.transformBy transform originalBounds
  let transformedVector = Vector2d.transformBy transform originalVector
  Test.expect (transformedVector ^ transformedBounds)
