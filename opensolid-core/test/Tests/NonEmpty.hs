module Tests.NonEmpty (tests) where

import Data.List qualified
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ pickMinimum
  , reverseMap
  , pairwise
  ]

pickMinimum :: Test
pickMinimum = Test.check 100 "pickMinimum" do
  values <- Test.generate (NonEmpty.random 20 (Random.int 0 100))
  let (minValue, remainingValues) = NonEmpty.pickMinimum values
  let minValueIsMin = List.all (>= minValue) remainingValues
  let orderIsUnchanged = remainingValues == Data.List.delete minValue (NonEmpty.toList values)
  Test.expect (minValueIsMin && orderIsUnchanged)
    & Test.output "minValueIsMin" minValueIsMin
    & Test.output "orderIsUnchanged" orderIsUnchanged
    & Test.output "values" values
    & Test.output "minValue" minValue
    & Test.output "remainingValues" remainingValues

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" do
  ints <- Test.generate (NonEmpty.random 10 (Random.int 1 10))
  let twice n = 2 * n
  Test.expect (NonEmpty.reverseMap twice ints == NonEmpty.reverse (NonEmpty.map twice ints))

pairwise :: Test
pairwise = Test.verify "pairwise" do
  let integers = NonEmpty.three 1 2 3
  let letters = NonEmpty.two 'A' 'B'
  let pairs = NonEmpty.pairwise (,) integers letters
  let expected = NonEmpty.six (1, 'A') (1, 'B') (2, 'A') (2, 'B') (3, 'A') (3, 'B')
  Test.expect (pairs == expected)
