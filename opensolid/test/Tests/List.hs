module Tests.List (tests) where

import List qualified
import OpenSolid
import Random
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ reverseMap
  ]

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" Test.do
  ints <- Random.list 10 (Random.int 1 10)
  let twice n = 2 * n
  Test.expect (List.reverseMap twice ints == List.reverse (List.map twice ints))
