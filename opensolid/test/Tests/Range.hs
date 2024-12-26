module Tests.Range (tests) where

import Length (Length)
import NonEmpty qualified
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import Parameter qualified
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import Range (Range)
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Units (Meters)

tests :: List Test
tests =
  [ smaller
  , larger
  , smallest
  , largest
  , qtyRangeDivision
  , rangeQtyDivision
  , rangeRangeDivision
  ]

smaller :: Test
smaller = Test.check 100 "smaller" Test.do
  range1 <- Range.random Random.length
  range2 <- Range.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Range.interpolate range1 t1
  let x2 = Range.interpolate range2 t2
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
larger = Test.check 100 "larger" Test.do
  range1 <- Range.random Random.length
  range2 <- Range.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Range.interpolate range1 t1
  let x2 = Range.interpolate range2 t2
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
valueInRange = Random.do
  range <- Range.random Random.length
  t <- Parameter.random
  let value = Range.interpolate range t
  Random.return (value, range)

smallest :: Test
smallest = Test.check 1000 "smallest" Test.do
  valuesAndRanges <- NonEmpty.random 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let smallestValue = Qty.smallest values
  let smallestRange = Range.smallest ranges
  Test.expect (Range.includes smallestValue smallestRange)

largest :: Test
largest = Test.check 1000 "largest" Test.do
  valuesAndRanges <- NonEmpty.random 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let largestValue = Qty.largest values
  let largestRange = Range.largest ranges
  Test.expect (Range.includes largestValue largestRange)
    |> Test.output "values" (Test.lines values)
    |> Test.output "ranges" (Test.lines ranges)
    |> Test.output "largestValue" largestValue
    |> Test.output "largestRange" largestRange

qtyRangeDivision :: Test
qtyRangeDivision = Test.check 1000 "qtyRangeDivision" Test.do
  x <- Random.length
  range <- Range.random Random.length
  t <- Parameter.random
  let y = Range.interpolate range t
  let quotient = x / y
  let rangeQuotient = x / range
  Test.expect (rangeQuotient |> Range.includes quotient)
    |> Test.output "x" x
    |> Test.output "range" range
    |> Test.output "y" y
    |> Test.output "quotient" quotient
    |> Test.output "rangeQuotient" rangeQuotient

rangeQtyDivision :: Test
rangeQtyDivision = Test.check 1000 "rangeQtyDivision" Test.do
  range <- Range.random Random.length
  t <- Parameter.random
  let x = Range.interpolate range t
  y <- Random.length
  let quotient = x / y
  let rangeQuotient = range / y
  Test.expect (rangeQuotient |> Range.includes quotient)
    |> Test.output "range" range
    |> Test.output "x" x
    |> Test.output "y" y
    |> Test.output "quotient" quotient
    |> Test.output "rangeQuotient" rangeQuotient

{-| TODO: have test helper for generally testing compatibility of
Range function with corresponding Qty function
-}
rangeRangeDivision :: Test
rangeRangeDivision = Test.check 1000 "rangeRangeDivision" Test.do
  range1 <- Range.random Random.length
  range2 <- Range.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Range.interpolate range1 t1
  let x2 = Range.interpolate range2 t2
  let quotient = x1 / x2
  let rangeQuotient = range1 / range2
  Test.expect (rangeQuotient |> Range.includes quotient)
    |> Test.output "range1" range1
    |> Test.output "range2" range2
    |> Test.output "x1" x1
    |> Test.output "x2" x2
    |> Test.output "quotient" quotient
    |> Test.output "rangeQuotient" rangeQuotient
