module Tests.Range (tests) where

import Debug qualified
import Length (Length)
import NonEmpty qualified
import OpenSolid
import Parameter1d qualified
import Qty qualified
import Random (Generator)
import Random qualified
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
  ]

smaller :: Test
smaller = Test.check 100 "smaller" $ do
  a <- Range.generator Random.length
  b <- Range.generator Random.length
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  let x = Range.interpolate a u
  let y = Range.interpolate b v
  let smallerValue = Qty.smaller x y
  let smallerRange = Range.smaller a b
  Test.expect (Range.includes smallerValue smallerRange) $
    [ "a: " ++ Debug.show a
    , "b: " ++ Debug.show b
    , "x: " ++ Debug.show x
    , "y: " ++ Debug.show y
    , "smallerValue: " ++ Debug.show smallerValue
    , "smallerRange: " ++ Debug.show smallerRange
    ]

larger :: Test
larger = Test.check 100 "larger" $ do
  a <- Range.generator Random.length
  b <- Range.generator Random.length
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  let x = Range.interpolate a u
  let y = Range.interpolate b v
  let largerValue = Qty.larger x y
  let largerRange = Range.larger a b
  Test.expect (Range.includes largerValue largerRange) $
    [ "a: " ++ Debug.show a
    , "b: " ++ Debug.show b
    , "x: " ++ Debug.show x
    , "y: " ++ Debug.show y
    , "largerValue: " ++ Debug.show largerValue
    , "largerRange: " ++ Debug.show largerRange
    ]

valueInRange :: Generator (Length, Range Meters)
valueInRange = do
  range <- Range.generator Random.length
  t <- Parameter1d.generator
  let value = Range.interpolate range t
  Random.return (value, range)

smallest :: Test
smallest = Test.check 1000 "smallest" $ do
  valuesAndRanges <- Random.nonEmpty 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let smallestValue = Qty.smallest values
  let smallestRange = Range.smallest ranges
  Test.expect (Range.includes smallestValue smallestRange) []

largest :: Test
largest = Test.check 1000 "largest" $ do
  valuesAndRanges <- Random.nonEmpty 5 valueInRange
  let (values, ranges) = NonEmpty.unzip2 valuesAndRanges
  let largestValue = Qty.largest values
  let largestRange = Range.largest ranges
  Test.expect (Range.includes largestValue largestRange) $
    [ Test.show "values" (Test.lines values)
    , Test.show "ranges" (Test.lines ranges)
    , Test.show "largestValue" largestValue
    , Test.show "largestRange" largestRange
    ]
