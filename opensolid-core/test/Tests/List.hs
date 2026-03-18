module Tests.List (tests) where

import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ reverseMap
  ]

reverseMap :: Test
reverseMap = Test.check 100 "reverseMap" do
  ints <- Test.generate (List.random 10 (Random.int 1 10))
  Test.expect (List.reverseMap (2 *) ints == List.reverse (List.map (2 *) ints))
