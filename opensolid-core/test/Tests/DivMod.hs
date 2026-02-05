module Tests.DivMod (tests) where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ intDiv
  , intMod
  , numberDiv
  , numberMod
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

numberDiv :: Test
numberDiv =
  Test.group "Number //" $
    [ Test.verify "1.7 // 0.5" (Test.expect (1.7 // 0.5 == 3))
    , Test.verify "-1.7 // 0.5" (Test.expect (-1.7 // 0.5 == -4))
    ]

numberMod :: Test
numberMod =
  Tolerance.using 1e-12 do
    Test.group "Number %" $
      [ Test.verify "1.7 % 0.5" (Test.expect (1.7 % 0.5 ~= 0.2))
      , Test.verify "-1.7 % 0.5" (Test.expect (-1.7 % 0.5 ~= 0.3))
      ]
