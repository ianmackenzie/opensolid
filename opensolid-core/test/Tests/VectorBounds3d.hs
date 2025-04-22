module Tests.VectorBounds3d (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import Test (Test)
import Test qualified
import Tests.Random qualified

tests :: Tolerance Meters => List Test
tests =
  [ magnitude
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
