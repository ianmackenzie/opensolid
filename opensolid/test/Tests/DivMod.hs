module Tests.DivMod (tests) where

import OpenSolid
import Test (Test)
import Test qualified
import Tolerance qualified

tests :: List Test
tests =
  [ intDiv
  , intMod
  , floatDiv
  , floatMod
  ]

intDiv :: Test
intDiv =
  Test.group "Int //" $
    [ Test.verify "10 // 3" (Test.expect (10 // 3 == 3))
    , Test.verify "-10 // 3" (Test.expect (-10 // 3 == -4))
    ]

intMod :: Test
intMod =
  Test.group "Int %" $
    [ Test.verify "10 % 3" (Test.expect (10 % 3 == 1))
    , Test.verify "-10 % 3" (Test.expect (-10 % 3 == 2))
    ]

floatDiv :: Test
floatDiv =
  Test.group "Float //" $
    [ Test.verify "1.7 // 0.5" (Test.expect (1.7 // 0.5 == 3))
    , Test.verify "-1.7 // 0.5" (Test.expect (-1.7 // 0.5 == -4))
    ]

floatMod :: Test
floatMod =
  Tolerance.using 1e-12 $
    Test.group "Float %" $
      [ Test.verify "1.7 % 0.5" (Test.expect (1.7 % 0.5 ~= 0.2))
      , Test.verify "-1.7 % 0.5" (Test.expect (-1.7 % 0.5 ~= 0.3))
      ]
