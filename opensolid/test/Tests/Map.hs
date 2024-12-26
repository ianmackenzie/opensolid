module Tests.Map (tests) where

import Map (Map)
import Map qualified
import OpenSolid.Prelude
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ foldl
  , foldr
  , take
  ]

simpleMap :: Map Int Text
simpleMap = Map.fromKeyValuePairs [(1, "a"), (2, "b"), (3, "c")]

foldl :: Test
foldl = Test.verify "foldl" Test.do
  let actual = Map.foldl (+) "" simpleMap
  let expected = "abc"
  Test.expect (actual == expected)

foldr :: Test
foldr = Test.verify "foldr" Test.do
  let actual = Map.foldr (+) "" simpleMap
  let expected = "abc"
  Test.expect (actual == expected)

take :: Test
take =
  Test.group "take" $
    [ Test.verify "exists" Test.do
        let actual = Map.take 2 simpleMap
        let expected = (Just "b", Map.fromKeyValuePairs [(1, "a"), (3, "c")])
        Test.expect (actual == expected)
    , Test.verify "does not exist" Test.do
        let actual = Map.take 4 simpleMap
        let expected = (Nothing, simpleMap)
        Test.expect (actual == expected)
    ]
