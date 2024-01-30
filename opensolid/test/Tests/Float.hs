module Tests.Float (tests) where

import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ exponentiation
  ]

exponentiation :: Test
exponentiation =
  Test.group "Exponentiation" $
    [ Test.verify "2.0 ** 3.0" (Test.expect $ 2.0 ** 3.0 ~= 8.0)
    , Test.verify "64.0 ** (1.0 / 3.0)" (Test.expect $ 64.0 ** (1.0 / 3.0) ~= 4.0)
    ]
 where
  ?tolerance = 1e-12
