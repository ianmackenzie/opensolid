module Tests.Interval (tests) where

import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
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
  , quantityIntervalDivision
  , intervalQuantityDivision
  , intervalIntervalDivision
  ]

smaller :: Test
smaller = Test.check 100 "smaller" Test.do
  interval1 <- Interval.random Random.length
  interval2 <- Interval.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Interval.interpolate interval1 t1
  let x2 = Interval.interpolate interval2 t2
  let smallerValue = Quantity.smaller x1 x2
  let smallerInterval = Interval.smaller interval1 interval2
  Test.expect (Interval.includes smallerValue smallerInterval)
    & Test.output "interval1" interval1
    & Test.output "interval2" interval2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "smallerValue" smallerValue
    & Test.output "smallerInterval" smallerInterval

larger :: Test
larger = Test.check 100 "larger" Test.do
  interval1 <- Interval.random Random.length
  interval2 <- Interval.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Interval.interpolate interval1 t1
  let x2 = Interval.interpolate interval2 t2
  let largerValue = Quantity.larger x1 x2
  let largerInterval = Interval.larger interval1 interval2
  Test.expect (Interval.includes largerValue largerInterval)
    & Test.output "interval1" interval1
    & Test.output "interval2" interval2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "largerValue" largerValue
    & Test.output "largerInterval" largerInterval

valueInInterval :: Generator (Length, Interval Meters)
valueInInterval = do
  interval <- Interval.random Random.length
  t <- Parameter.random
  let value = Interval.interpolate interval t
  Random.return (value, interval)

smallest :: Test
smallest = Test.check 1000 "smallest" Test.do
  valuesAndIntervals <- NonEmpty.random 5 valueInInterval
  let (values, intervals) = NonEmpty.unzip2 valuesAndIntervals
  let smallestValue = Quantity.smallest values
  let smallestInterval = Interval.smallest intervals
  Test.expect (Interval.includes smallestValue smallestInterval)

largest :: Test
largest = Test.check 1000 "largest" Test.do
  valuesAndIntervals <- NonEmpty.random 5 valueInInterval
  let (values, intervals) = NonEmpty.unzip2 valuesAndIntervals
  let largestValue = Quantity.largest values
  let largestInterval = Interval.largest intervals
  Test.expect (Interval.includes largestValue largestInterval)
    & Test.output "values" (Test.lines (NonEmpty.toList values))
    & Test.output "intervals" (Test.lines (NonEmpty.toList intervals))
    & Test.output "largestValue" largestValue
    & Test.output "largestInterval" largestInterval

quantityIntervalDivision :: Test
quantityIntervalDivision = Test.check 1000 "quantityIntervalDivision" Test.do
  x <- Random.length
  interval <- Interval.random Random.length
  t <- Parameter.random
  let y = Interval.interpolate interval t
  let quotient = x ./. y
  let intervalQuotient = x ./. interval
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "x" x
    & Test.output "interval" interval
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient

intervalQuantityDivision :: Test
intervalQuantityDivision = Test.check 1000 "intervalQuantityDivision" Test.do
  interval <- Interval.random Random.length
  t <- Parameter.random
  let x = Interval.interpolate interval t
  y <- Random.length
  let quotient = x ./. y
  let intervalQuotient = interval ./. y
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "interval" interval
    & Test.output "x" x
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient

{-| TODO: have test helper for generally testing compatibility of
Interval function with corresponding Quantity function
-}
intervalIntervalDivision :: Test
intervalIntervalDivision = Test.check 1000 "intervalIntervalDivision" Test.do
  interval1 <- Interval.random Random.length
  interval2 <- Interval.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Interval.interpolate interval1 t1
  let x2 = Interval.interpolate interval2 t2
  let quotient = x1 ./. x2
  let intervalQuotient = interval1 ./. interval2
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "interval1" interval1
    & Test.output "interval2" interval2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient
