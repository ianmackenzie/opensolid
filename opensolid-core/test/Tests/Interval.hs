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
quantityIntervalDivision = Test.check 1000 "quantityIntervalDivision" Test.do
  x <- Random.length
  interval <- Interval.random Random.length
  t <- Parameter.random
  let y = Interval.interpolate interval t
  let quotient = x / y
  let intervalQuotient = x / interval
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "x" x
    & Test.output "interval" interval
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient

intervalQuantityDivision :: Test
intervalQuantityDivision = Test.check 1000 "intervalQuantityDivision" Test.do
  interval <- Interval.random Random.length
  t <- Parameter.random
  let x = Interval.interpolate interval t
  y <- Random.length
  let quotient = x / y
  let intervalQuotient = interval / y
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "interval" interval
    & Test.output "x" x
    & Test.output "y" y
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient

{-| TODO: have test helper for generally testing compatibility of
Interval function with corresponding Quantity function
-}
intervalIntervalDivision :: Test
intervalIntervalDivision = Test.check 1000 "intervalIntervalDivision" Test.do
  interval1 <- Interval.random Random.length
  interval2 <- Interval.random Random.length
  t1 <- Parameter.random
  t2 <- Parameter.random
  let x1 = Interval.interpolate interval1 t1
  let x2 = Interval.interpolate interval2 t2
  let quotient = x1 / x2
  let intervalQuotient = interval1 / interval2
  Test.expect (Interval.includes quotient intervalQuotient)
    & Test.output "interval1" interval1
    & Test.output "interval2" interval2
    & Test.output "x1" x1
    & Test.output "x2" x2
    & Test.output "quotient" quotient
    & Test.output "intervalQuotient" intervalQuotient
