module Tests.Float (tests) where

import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ exponentiation
  ]
 where
  ?tolerance = 1e-12

exponentiation :: Tolerance Unitless => Test
exponentiation =
  Test.group "Exponentiation" $
    [ Test.verify "2.0 ** 3" (Test.expect (2.0 ** 3 ~= 8.0))
    , Test.verify "64.0 ** (1 / 3)" (Test.expect (64.0 ** (1 / 3) ~= 4.0))
    , Test.verify "2.0 ** -3" (Test.expect (2.0 ** -3 ~= 0.125))
    ]
