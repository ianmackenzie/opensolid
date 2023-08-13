module Tests.NonEmpty (tests) where

import Data.List qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Random qualified
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ takeMinimum
  ]

takeMinimum :: Test
takeMinimum =
  Test.check 100 "takeMinimum" $ do
    values <- Random.nonEmpty 20 (Random.int 0 100)
    let (minValue, remainingValues) = NonEmpty.takeMinimum values
    let minValueIsMin = List.all (>= minValue) remainingValues
    let orderIsUnchanged = remainingValues == Data.List.delete minValue (NonEmpty.toList values)
    Test.expect (minValueIsMin && orderIsUnchanged)
      |> Test.output "minValueIsMin" minValueIsMin
      |> Test.output "orderIsUnchanged" orderIsUnchanged
      |> Test.output "values" values
      |> Test.output "minValue" minValue
      |> Test.output "remainingValues" remainingValues
