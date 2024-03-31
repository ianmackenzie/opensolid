module Tests.Dict (tests) where

import Dict (Dict)
import Dict qualified
import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ foldl
  , foldr
  , take
  ]

simpleDict :: Dict Int String
simpleDict = Dict.fromList [(1, "a"), (2, "b"), (3, "c")]

foldl :: Test
foldl = Test.verify "foldl" Test.do
  let actual = Dict.foldl (+) "" simpleDict
  let expected = "abc"
  Test.expect (actual == expected)

foldr :: Test
foldr = Test.verify "foldr" Test.do
  let actual = Dict.foldr (+) "" simpleDict
  let expected = "abc"
  Test.expect (actual == expected)

take :: Test
take =
  Test.group "take" $
    [ Test.verify "exists" Test.do
        let actual = Dict.take 2 simpleDict
        let expected = (Just "b", Dict.fromList [(1, "a"), (3, "c")])
        Test.expect (actual == expected)
    , Test.verify "does not exist" Test.do
        let actual = Dict.take 4 simpleDict
        let expected = (Nothing, simpleDict)
        Test.expect (actual == expected)
    ]
