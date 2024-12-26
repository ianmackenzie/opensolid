module Tests.VectorBounds3d (tests) where

import OpenSolid.Prelude
import OpenSolid.Vector3d qualified as Vector3d
import Parameter qualified
import Test (Test)
import Test qualified
import Tests.Random qualified
import Units (Meters)
import VectorBounds3d qualified

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
  let magnitudeRange = VectorBounds3d.magnitude vectorBounds
  Test.expect (vectorMagnitude ^ magnitudeRange)
