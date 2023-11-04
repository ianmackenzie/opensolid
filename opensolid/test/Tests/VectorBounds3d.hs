module Tests.VectorBounds3d (tests) where

import OpenSolid
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified
import U qualified
import Units (Meters)
import Vector3d qualified
import VectorBounds3d qualified

tests :: (Tolerance Meters) => List Test
tests =
  [ magnitude
  ]

magnitude :: (Tolerance Meters) => Test
magnitude = Test.check 100 "magnitude" $ Test.do
  vectorBounds <- Tests.Random.vectorBounds3d
  ux <- U.generator
  uy <- U.generator
  uz <- U.generator
  let vector = VectorBounds3d.interpolate vectorBounds ux uy uz
  let vectorMagnitude = Vector3d.magnitude vector
  let magnitudeRange = VectorBounds3d.magnitude vectorBounds
  Test.expect (Range.approximatelyIncludes vectorMagnitude magnitudeRange)
