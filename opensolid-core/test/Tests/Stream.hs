module Tests.Stream (tests) where

import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Stream qualified as Stream
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

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
  Tolerance.using 1e-3 do
    Test.expect (sum ~= 2.0)
      |> Test.output "sum" sum
      |> Test.output "error" (sum - 2.0)
