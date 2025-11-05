module Tests.Number (tests) where

import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding ((/))
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified
import Prelude ((/))

tests :: List Test
tests =
  Tolerance.using 1e-12 $
    [ exponentiation
    ]

exponentiation :: Tolerance Unitless => Test
exponentiation =
  Test.group "Exponentiation" $
    [ Test.verify "2.0 ** 3" (Test.expect (Number.pow 2.0 3.0 ~= 8.0))
    , Test.verify "64.0 ** (1 / 3)" (Test.expect (Number.pow 64.0 (1.0 / 3.0) ~= 4.0))
    , Test.verify "2.0 ** -3" (Test.expect (Number.pow 2.0 -3.0 ~= 0.125))
    ]
