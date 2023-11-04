module Tests.Range (tests) where

import Float qualified
import Length (Length)
import NonEmpty qualified
import OpenSolid
import Qty qualified
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import U qualified
import Units (Meters)

tests :: List Test
tests =
  [ smaller
  , larger
  , smallest
  , largest
  , find
  ]

smaller :: Test
smaller = Test.check 100 "smaller" $ Test.do
  range1 <- Range.generator Random.length
  range2 <- Range.generator Random.length
  u1 <- U.generator
  u2 <- U.generator
  let x1 = Range.interpolate range1 u1
  let x2 = Range.interpolate range2 u2
  let smallerValue = Qty.smaller x1 x2
  let smallerRange = Range.smaller range1 range2
  Test.expect (Range.includes smallerValue smallerRange)
    |> Test.output "range1" range1
    |> Test.output "range2" range2
    |> Test.output "x1" x1
    |> Test.output "x2" x2
    |> Test.output "smallerValue" smallerValue
    |> Test.output "smallerRange" smallerRange

larger :: Test
larger = Test.check 100 "larger" $ Test.do
  range1 <- Range.generator Random.length
  range2 <- Range.generator Random.length
  u1 <- U.generator
  u2 <- U.generator
  let x1 = Range.interpolate range1 u1
  let x2 = Range.interpolate range2 u2
  let largerValue = Qty.larger x1 x2
  let largerRange = Range.larger range1 range2
  Test.expect (Range.includes largerValue largerRange)
    |> Test.output "range1" range1
    |> Test.output "range2" range2
    |> Test.output "x1" x1
    |> Test.output "x2" x2
    |> Test.output "largerValue" largerValue
    |> Test.output "largerRange" largerRange

valueInRange :: Generator (Length, Range Meters)
valueInRange = Test.do
  range <- Range.generator Random.length
  u <- U.generator
  let value = Range.interpolate range u
  Random.return (value, range)

smallest :: Test
smallest = Test.check 1000 "smallest" $ Test.do
  valuesAndRanges <- Random.nonEmpty 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let smallestValue = Qty.smallest values
  let smallestRange = Range.smallest ranges
  Test.expect (Range.includes smallestValue smallestRange)

largest :: Test
largest = Test.check 1000 "largest" $ Test.do
  valuesAndRanges <- Random.nonEmpty 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let largestValue = Qty.largest values
  let largestRange = Range.largest ranges
  Test.expect (Range.includes largestValue largestRange)
    |> Test.output "values" (Test.lines values)
    |> Test.output "ranges" (Test.lines ranges)
    |> Test.output "largestValue" largestValue
    |> Test.output "largestRange" largestRange

find :: Test
find =
  Test.check 1 "find" $
    let isRoot x = Range.includes 2.0 (x * x)
     in case Range.find isRoot (Range.from 1.0 2.0) of
          Nothing -> Test.fail "Should have found the square root of 2 between 1.0 and 2.0"
          Just root -> Test.expect (root ~= Float.sqrt 2.0) where ?tolerance = 1e-12
