module Tests.List (tests) where

import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ reverseMap
  ]

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" Test.do
  ints <- List.random 10 (Int.random 1 10)
  let twice n = 2 * n
  Test.expect (List.reverseMap twice ints == List.reverse (List.map twice ints))
