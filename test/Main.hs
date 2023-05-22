module Main (main) where

import Length (Length)
import Length qualified
import OpenSolid
import Parameter1d qualified
import Qty qualified
import Random (Generator)
import Random qualified
import Range qualified
import Task qualified
import Test (Test)
import Test qualified
import Units (Meters)
import Vector3d qualified
import VectorBox3d (VectorBox3d (VectorBox3d))
import VectorBox3d qualified
import Prelude qualified

randomLength :: Generator Length
randomLength = Random.qtyFrom Qty.zero (Length.meters 10.0)

randomVectorBox3d :: Generator (VectorBox3d (space @ Meters))
randomVectorBox3d = Prelude.do
  x <- Range.generator randomLength
  y <- Range.generator randomLength
  z <- Range.generator randomLength
  Random.return (VectorBox3d x y z)

testVectorBox3dMagnitude :: Tolerance Meters => Test
testVectorBox3dMagnitude = Test.check 100 $ do
  vectorBox <- randomVectorBox3d
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  w <- Parameter1d.generator
  let vector = VectorBox3d.interpolate vectorBox u v w
  let magnitude = Vector3d.magnitude vector
  let magnitudeRange = VectorBox3d.magnitude vectorBox
  Random.return (Test.rangeApproximatelyContains magnitude magnitudeRange)

tests :: Test
tests =
  Test.group "Tests" $
    [ testVectorBox3dMagnitude
    ]
 where
  ?tolerance = Length.meters 1e-12

main :: IO ()
main = Task.toIO (Test.run tests)
