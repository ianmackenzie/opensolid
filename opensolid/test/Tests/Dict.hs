module Tests.Dict (tests) where

import Dict (Dict)
import Dict qualified
import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ foldLeft
  , foldRight
  , take
  ]

simpleDict :: Dict Int String
simpleDict = Dict.fromList [(1, "a"), (2, "b"), (3, "c")]

foldLeft :: Test
foldLeft = Test.verify "foldLeft" $ Test.do
  let actual = Dict.foldLeft (++) "" simpleDict
      expected = "abc"
   in Test.expect (actual == expected)

foldRight :: Test
foldRight = Test.verify "foldRight" $ Test.do
  let actual = Dict.foldRight (++) "" simpleDict
      expected = "abc"
   in Test.expect (actual == expected)

take :: Test
take =
  Test.group "take" $
    [ Test.verify "exists" $ Test.do
        let actual = Dict.take 2 simpleDict
            expected = (Just "b", Dict.fromList [(1, "a"), (3, "c")])
         in Test.expect (actual == expected)
    , Test.verify "does not exist" $ Test.do
        let actual = Dict.take 4 simpleDict
            expected = (Nothing, simpleDict)
         in Test.expect (actual == expected)
    ]
