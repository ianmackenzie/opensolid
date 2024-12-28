module Tests.Array (tests) where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ reverse
  , reverseMap
  , interpolate
  , aggregate
  ]

reverse :: Test
reverse =
  Test.verify "reverse" Test.do
    let array = Array.new (NonEmpty.of3 1 2 3)
    let reversed = Array.reverse array
    Test.expect (NonEmpty.toList (Array.items reversed) == [3, 2, 1])

reverseMap :: Test
reverseMap =
  Test.verify "reverse" Test.do
    let array = Array.new (NonEmpty.of3 1 2 3)
    let reversed = Array.reverseMap Text.int array
    Test.expect (NonEmpty.toList (Array.items reversed) == ["3", "2", "1"])

ranges :: Array (Range Unitless)
ranges =
  Array.new $
    NonEmpty.of3
      (Range.from 10.0 20.0)
      (Range.from 20.0 30.0)
      (Range.from 30.0 40.0)

interpolate :: Test
interpolate =
  Tolerance.using 1e-12 $
    Test.verify "interpolate" Test.do
      let check t expected = do
            let actual = Array.interpolate Range.interpolate ranges t
            Test.expect (actual ~= expected)
              |> Test.output "t" t
              |> Test.output "expected" expected
              |> Test.output "actual" actual

      Test.all
        [ check 0.0 10.0
        , check 1.0 40.0
        , check 0.5 25.0
        , check 0.1 13.0
        , check 0.9 37.0
        ]

interpolateBounds :: Range units -> Range Unitless -> Range units
interpolateBounds r t =
  Range.from
    (Range.interpolate r (Range.lowerBound t))
    (Range.interpolate r (Range.upperBound t))

aggregate :: Test
aggregate =
  Tolerance.using 1e-12 $
    Test.verify "interpolate" Test.do
      let check t expected = do
            let actual = Array.aggregate interpolateBounds ranges t
            Test.expect (Range.endpoints actual ~= Range.endpoints expected)
              |> Test.output "t" t
              |> Test.output "expected" expected
              |> Test.output "actual" actual

      Test.all
        [ check Range.unit (Range.from 10.0 40.0)
        , check (Range.from 0.1 0.2) (Range.from 13.0 16.0)
        , check (Range.from 0.1 0.9) (Range.from 13.0 37.0)
        , check (Range.from 0.3 0.4) (Range.from 19.0 22.0)
        ]