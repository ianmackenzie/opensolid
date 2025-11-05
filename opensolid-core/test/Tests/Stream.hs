module Tests.Stream (tests) where

import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding ((-))
import OpenSolid.Stream qualified as Stream
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified
import Prelude ((-))

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
          |> Number.sum
  let expected :: Number = 2.0
  Tolerance.using 1e-3 do
    Test.expect (sum ~= expected)
      |> Test.output "sum" sum
      |> Test.output "error" (sum - 2.0)
