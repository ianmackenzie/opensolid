module Tests.Array (tests) where

import OpenSolid.Array qualified as Array
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ reverse
  , reverseMap
  ]

reverse :: Test
reverse =
  Test.verify "reverse" Test.do
    let array = Array.new (NonEmpty.three 1 2 3)
    let reversed = Array.reverse array
    Test.expect (NonEmpty.toList (Array.items reversed) == [3, 2, 1])

reverseMap :: Test
reverseMap =
  Test.verify "reverse" Test.do
    let array = Array.new (NonEmpty.three 1 2 3)
    let reversed = Array.reverseMap Text.int array
    Test.expect (NonEmpty.toList (Array.items reversed) == ["3", "2", "1"])
