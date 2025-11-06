module Tests.List (tests) where

import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import Test (Test)
import Test qualified
import Prelude ((*))

tests :: List Test
tests =
  [ reverseMap
  ]

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" Test.do
  ints <- List.random 10 (Int.random 1 10)
  Test.expect (List.reverseMap (2 *) ints == List.reverse (List.map (2 *) ints))
