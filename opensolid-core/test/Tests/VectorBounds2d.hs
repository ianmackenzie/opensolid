module Tests.VectorBounds2d (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ placeIn
  , relativeTo
  ]

placeIn :: Test
placeIn = Test.check 100 "placeIn" Test.do
  localBounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let localVector = VectorBounds2d.interpolate localBounds u v
  basis <- Random.basis2d
  let globalBounds = VectorBounds2d.placeIn basis localBounds
  let globalVector = Vector2d.placeIn basis localVector
  Test.expect (VectorBounds2d.includes globalVector globalBounds)

relativeTo :: Test
relativeTo = Test.check 100 "relativeTo" Test.do
  globalBounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let globalVector = VectorBounds2d.interpolate globalBounds u v
  basis <- Random.basis2d
  let localBounds = VectorBounds2d.relativeTo basis globalBounds
  let localVector = Vector2d.relativeTo basis globalVector
  Test.expect (VectorBounds2d.includes localVector localBounds)
