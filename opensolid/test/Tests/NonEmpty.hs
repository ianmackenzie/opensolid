module Tests.NonEmpty (tests) where

import Data.List qualified
import Int qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ pickMinimum
  , reverseMap
  ]

pickMinimum :: Test
pickMinimum = Test.check 100 "pickMinimum" Test.do
  values <- NonEmpty.random 20 (Int.random 0 100)
  let (minValue, remainingValues) = NonEmpty.pickMinimum values
  let minValueIsMin = List.allSatisfy (>= minValue) remainingValues
  let orderIsUnchanged = remainingValues == Data.List.delete minValue (NonEmpty.toList values)
  Test.expect (minValueIsMin && orderIsUnchanged)
    |> Test.output "minValueIsMin" minValueIsMin
    |> Test.output "orderIsUnchanged" orderIsUnchanged
    |> Test.output "values" values
    |> Test.output "minValue" minValue
    |> Test.output "remainingValues" remainingValues

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" Test.do
  ints <- NonEmpty.random 10 (Int.random 1 10)
  let twice n = 2 * n
  Test.expect (NonEmpty.reverseMap twice ints == NonEmpty.reverse (NonEmpty.map twice ints))
