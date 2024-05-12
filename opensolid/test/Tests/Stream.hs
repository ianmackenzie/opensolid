module Tests.Stream (tests) where

import Float qualified
import OpenSolid
import Stream qualified
import Test (Test)
import Test qualified
import Tolerance qualified

tests :: List Test
tests =
  [ mapWithIndex
  ]

mapWithIndex :: Test
mapWithIndex = Test.verify "mapWithIndex" Test.do
  let sum =
        Stream.repeat 1
          |> Stream.mapWithIndex (\i n -> n / (2 ** i))
          |> Stream.take 11
          |> Float.sum
  Test.expect (Tolerance.using 1e-3 (sum ~= 2.0))
    |> Test.output "sum" sum
    |> Test.output "error" (sum - 2.0)
