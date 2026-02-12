module Tests.Number (tests) where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  Tolerance.using 1e-12 $
    [ exponentiation
    ]

exponentiation :: Tolerance Unitless => Test
exponentiation =
  Test.group "Exponentiation" $
    [ Test.verify "2 ** 3" (Test.expect (2 ** 3 == 8))
    , Test.verify "64. ** (1 / 3)" (Test.expect (64.0 ** (1 / 3) ~= 4.0))
    , Test.verify "2.0 ** -3.0" (Test.expect (2.0 ** -3.0 ~= 0.125))
    ]
