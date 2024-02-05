module Tests.VectorBounds3d (tests) where

import OpenSolid
import T qualified
import Test (Test)
import Test qualified
import Tests.Random qualified
import Units (Meters)
import Vector3d qualified
import VectorBounds3d qualified

tests :: Tolerance Meters => List Test
tests =
  [ magnitude
  ]

magnitude :: Tolerance Meters => Test
magnitude =
  Test.check 100 "magnitude" <| Test.do
    vectorBounds <- Tests.Random.vectorBounds3d
    tx <- T.generator
    ty <- T.generator
    tz <- T.generator
    let vector = VectorBounds3d.interpolate vectorBounds tx ty tz
    let vectorMagnitude = Vector3d.magnitude vector
    let magnitudeRange = VectorBounds3d.magnitude vectorBounds
    Test.expect (vectorMagnitude ^ magnitudeRange)
