module Tests.VectorBounds3d (tests) where

import OpenSolid
import Parameter1d qualified
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified
import Units (Meters)
import Vector3d qualified
import VectorBounds3d qualified

tests :: (Tolerance Meters) => List Test
tests =
  [ magnitude
  ]

magnitude :: (Tolerance Meters) => Test
magnitude = Test.check 100 "magnitude" $ do
  vectorBounds <- Tests.Random.vectorBounds3d
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  w <- Parameter1d.generator
  let vector = VectorBounds3d.interpolate vectorBounds u v w
  let vectorMagnitude = Vector3d.magnitude vector
  let magnitudeRange = VectorBounds3d.magnitude vectorBounds
  Test.expect (Range.approximatelyIncludes vectorMagnitude magnitudeRange)
