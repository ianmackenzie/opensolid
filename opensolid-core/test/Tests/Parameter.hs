module Tests.Parameter (tests) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ steps
  , leading
  , trailing
  , inBetween
  , midpoints
  ]

check :: (Int -> List Number) -> Int -> List Number -> Test
check function n expected =
  Test.verify (Text.int n) Test.do
    Test.expect (function n == expected)
      & Test.output "expected" expected
      & Test.output "actual" (function n)

steps :: Test
steps =
  Test.group "steps" $
    [ check Parameter.steps 0 []
    , check Parameter.steps 1 [0.0, 1.0]
    , check Parameter.steps 2 [0.0, 0.5, 1.0]
    , check Parameter.steps 5 [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
    ]

leading :: Test
leading =
  Test.group "leading" $
    [ check Parameter.leading 0 []
    , check Parameter.leading 1 [0.0]
    , check Parameter.leading 2 [0.0, 0.5]
    , check Parameter.leading 5 [0.0, 0.2, 0.4, 0.6, 0.8]
    ]

trailing :: Test
trailing =
  Test.group "trailing" $
    [ check Parameter.trailing 0 []
    , check Parameter.trailing 1 [1.0]
    , check Parameter.trailing 2 [0.5, 1.0]
    , check Parameter.trailing 5 [0.2, 0.4, 0.6, 0.8, 1.0]
    ]

inBetween :: Test
inBetween =
  Test.group "inBetween" $
    [ check Parameter.inBetween 0 []
    , check Parameter.inBetween 1 []
    , check Parameter.inBetween 2 [0.5]
    , check Parameter.inBetween 5 [0.2, 0.4, 0.6, 0.8]
    ]

midpoints :: Test
midpoints =
  Test.group "midpoints" $
    [ check Parameter.midpoints 0 []
    , check Parameter.midpoints 1 [0.5]
    , check Parameter.midpoints 2 [0.25, 0.75]
    , check Parameter.midpoints 5 [0.1, 0.3, 0.5, 0.7, 0.9]
    ]
