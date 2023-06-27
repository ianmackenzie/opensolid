module Tests.VectorBox3d (tests) where

import Expect qualified
import OpenSolid
import Parameter1d qualified
import Random qualified
import Test (Test)
import Test qualified
import Tests.Random qualified
import Units (Meters)
import Vector3d qualified
import VectorBox3d qualified

tests :: Tolerance Meters => List Test
tests =
  [ magnitude
  ]

magnitude :: Tolerance Meters => Test
magnitude = Test.check 100 "magnitude" $ do
  vectorBox <- Tests.Random.vectorBox3d
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  w <- Parameter1d.generator
  let vector = VectorBox3d.interpolate vectorBox u v w
  let vectorMagnitude = Vector3d.magnitude vector
  let magnitudeRange = VectorBox3d.magnitude vectorBox
  Random.return (Expect.rangeApproximatelyIncludes vectorMagnitude magnitudeRange)
