module Tests.Number (tests) where

import OpenSolid.Number qualified as Number
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
    [ Test.verify "2 ^ 3" (Test.expect (Number.pow 2 3 ~= 8))
    , Test.verify "64 ^ (1 / 3)" (Test.expect (Number.pow 64 (1 / 3) ~= 4))
    , Test.verify "2 ^ -3" (Test.expect (Number.pow 2 -3 ~= 0.125))
    ]
