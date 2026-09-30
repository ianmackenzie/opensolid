module Tests.Interval (tests) where

import OpenSolid.Interval qualified as Interval
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ quantityIntervalDivision
  , intervalQuantityDivision
  , intervalIntervalDivision
  ]

quantityIntervalDivision :: Test
quantityIntervalDivision = Test.check 1000 "quantityIntervalDivision" do
  x <- Test.generate Random.length
  interval <- Test.generate (Interval.random Random.length)
  tValue <- Test.generate Parameter.random
  let y = interval @ tValue
  let quotient = x / y
  let intervalQuotient = x / interval
  Test.expect (Interval.member quotient intervalQuotient)
    & Test.log {x, interval, y, quotient, intervalQuotient}

intervalQuantityDivision :: Test
intervalQuantityDivision = Test.check 1000 "intervalQuantityDivision" do
  interval <- Test.generate (Interval.random Random.length)
  tValue <- Test.generate Parameter.random
  let x = interval @ tValue
  y <- Test.generate Random.length
  let quotient = x / y
  let intervalQuotient = interval / y
  Test.expect (Interval.member quotient intervalQuotient)
    & Test.log {interval, x, y, quotient, intervalQuotient}

{-| TODO: have test helper for generally testing compatibility of
Interval function with corresponding Quantity function
-}
intervalIntervalDivision :: Test
intervalIntervalDivision = Test.check 1000 "intervalIntervalDivision" do
  interval1 <- Test.generate (Interval.random Random.length)
  interval2 <- Test.generate (Interval.random Random.length)
  t1 <- Test.generate Parameter.random
  t2 <- Test.generate Parameter.random
  let x1 = interval1 @ t1
  let x2 = interval2 @ t2
  let quotient = x1 / x2
  let intervalQuotient = interval1 / interval2
  Test.expect (Interval.member quotient intervalQuotient)
    & Test.log {interval1, interval2, x1, x2, quotient, intervalQuotient}
