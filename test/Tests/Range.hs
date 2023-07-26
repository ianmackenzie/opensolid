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
  [ smaller
  , larger
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
