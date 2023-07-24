module Tests.Range (tests) where

import Debug qualified
import OpenSolid
import Parameter1d qualified
import Qty qualified
import Range qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ smallest
  , largest
  ]

smallest :: Test
smallest = Test.check 100 "smallest" $ do
  a <- Range.generator Random.length
  b <- Range.generator Random.length
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  let x = Range.interpolate a u
  let y = Range.interpolate b v
  let smallestValue = Qty.smallest x y
  let smallestRange = Range.smallest a b
  Test.expect (Range.includes smallestValue smallestRange) $
    [ "a: " ++ Debug.show a
    , "b: " ++ Debug.show b
    , "x: " ++ Debug.show x
    , "y: " ++ Debug.show y
    , "smallestValue: " ++ Debug.show smallestValue
    , "smallestRange: " ++ Debug.show smallestRange
    ]

largest :: Test
largest = Test.check 100 "largest" $ do
  a <- Range.generator Random.length
  b <- Range.generator Random.length
  u <- Parameter1d.generator
  v <- Parameter1d.generator
  let x = Range.interpolate a u
  let y = Range.interpolate b v
  let largestValue = Qty.largest x y
  let largestRange = Range.largest a b
  Test.expect (Range.includes largestValue largestRange) $
    [ "a: " ++ Debug.show a
    , "b: " ++ Debug.show b
    , "x: " ++ Debug.show x
    , "y: " ++ Debug.show y
    , "largestValue: " ++ Debug.show largestValue
    , "largestRange: " ++ Debug.show largestRange
    ]
