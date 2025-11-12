module Tests.Bounds (tests) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Length (Length)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ smaller
  , larger
  , smallest
  , largest
  , quantityBoundsDivision
  , boundsQuantityDivision
  , boundsBoundsDivision
  ]

smaller :: Test
smaller = Test.check 100 "smaller" Test.do
  bounds1 <- Bounds.random Random.length
  bounds2 <- Bounds.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Bounds.interpolate bounds1 t1
  let x2 = Bounds.interpolate bounds2 t2
  let smallerValue = Quantity.smaller x1 x2
  let smallerBounds = Bounds.smaller bounds1 bounds2
  Test.expect (Bounds.includes smallerValue smallerBounds)
    & Test.output "bounds1" bounds1
    & Test.output "bounds2" bounds2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "smallerValue" smallerValue
    & Test.output "smallerBounds" smallerBounds

larger :: Test
larger = Test.check 100 "larger" Test.do
  bounds1 <- Bounds.random Random.length
  bounds2 <- Bounds.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Bounds.interpolate bounds1 t1
  let x2 = Bounds.interpolate bounds2 t2
  let largerValue = Quantity.larger x1 x2
  let largerBounds = Bounds.larger bounds1 bounds2
  Test.expect (Bounds.includes largerValue largerBounds)
    & Test.output "bounds1" bounds1
    & Test.output "bounds2" bounds2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "largerValue" largerValue
    & Test.output "largerBounds" largerBounds

valueInBounds :: Generator (Length, Bounds Meters)
valueInBounds = do
  bounds <- Bounds.random Random.length
  t <- Parameter.random
  let value = Bounds.interpolate bounds t
  Random.return (value, bounds)

smallest :: Test
smallest = Test.check 1000 "smallest" Test.do
  valuesAndBounds <- NonEmpty.random 5 valueInBounds
  let (values, bounds) = NonEmpty.unzip2 valuesAndBounds
  let smallestValue = Quantity.smallest values
  let smallestBounds = Bounds.smallest bounds
  Test.expect (Bounds.includes smallestValue smallestBounds)

largest :: Test
largest = Test.check 1000 "largest" Test.do
  valuesAndBounds <- NonEmpty.random 5 valueInBounds
  let (values, bounds) = NonEmpty.unzip2 valuesAndBounds
  let largestValue = Quantity.largest values
  let largestBounds = Bounds.largest bounds
  Test.expect (Bounds.includes largestValue largestBounds)
    & Test.output "values" (Test.lines values)
    & Test.output "bounds" (Test.lines bounds)
    & Test.output "largestValue" largestValue
    & Test.output "largestBounds" largestBounds

quantityBoundsDivision :: Test
quantityBoundsDivision = Test.check 1000 "quantityBoundsDivision" Test.do
  x <- Random.length
  bounds <- Bounds.random Random.length
  t <- Parameter.random
  let y = Bounds.interpolate bounds t
  let quotient = x ./. y
  let boundsQuotient = x ./. bounds
  Test.expect (Bounds.includes quotient boundsQuotient)
    & Test.output "x" x
    & Test.output "bounds" bounds
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "boundsQuotient" boundsQuotient

boundsQuantityDivision :: Test
boundsQuantityDivision = Test.check 1000 "boundsQuantityDivision" Test.do
  bounds <- Bounds.random Random.length
  t <- Parameter.random
  let x = Bounds.interpolate bounds t
  y <- Random.length
  let quotient = x ./. y
  let boundsQuotient = bounds ./. y
  Test.expect (Bounds.includes quotient boundsQuotient)
    & Test.output "bounds" bounds
    & Test.output "x" x
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "boundsQuotient" boundsQuotient

{-| TODO: have test helper for generally testing compatibility of
Bounds function with corresponding Quantity function
-}
boundsBoundsDivision :: Test
boundsBoundsDivision = Test.check 1000 "boundsBoundsDivision" Test.do
  bounds1 <- Bounds.random Random.length
  bounds2 <- Bounds.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Bounds.interpolate bounds1 t1
  let x2 = Bounds.interpolate bounds2 t2
  let quotient = x1 ./. x2
  let boundsQuotient = bounds1 ./. bounds2
  Test.expect (Bounds.includes quotient boundsQuotient)
    & Test.output "bounds1" bounds1
    & Test.output "bounds2" bounds2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "quotient" quotient
    & Test.output "boundsQuotient" boundsQuotient
