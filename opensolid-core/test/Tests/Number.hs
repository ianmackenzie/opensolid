module Tests.Number (tests) where

import OpenSolid.Prelude
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ exponentiation
  ]

exponentiation :: Test
exponentiation =
  Test.group "Exponentiation" $
    [ Test.verify "2 ** 3" (Test.expect (unitless (2 ** 3 == 8)))
    , Test.verify "64. ** (1 / 3)" (Test.expect (unitless (64.0 ** (1 / 3) ~= 4.0)))
    , Test.verify "2.0 ** -3.0" (Test.expect (unitless (2.0 ** -3.0 ~= 0.125)))
    ]
