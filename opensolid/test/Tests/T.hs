module Tests.T (tests) where

import OpenSolid
import String qualified
import T qualified
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

check :: (Int -> List Float) -> Int -> List Float -> Test
check function n expected =
  Test.verify (String.fromInt n) $ Test.do
    Test.expect (function n == expected)
      |> Test.output "expected" expected
      |> Test.output "actual" (function n)

steps :: Test
steps =
  Test.group "steps" $
    [ check T.steps 0 []
    , check T.steps 1 [0.0, 1.0]
    , check T.steps 2 [0.0, 0.5, 1.0]
    , check T.steps 5 [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
    ]

leading :: Test
leading =
  Test.group "leading" $
    [ check T.leading 0 []
    , check T.leading 1 [0.0]
    , check T.leading 2 [0.0, 0.5]
    , check T.leading 5 [0.0, 0.2, 0.4, 0.6, 0.8]
    ]

trailing :: Test
trailing =
  Test.group "trailing" $
    [ check T.trailing 0 []
    , check T.trailing 1 [1.0]
    , check T.trailing 2 [0.5, 1.0]
    , check T.trailing 5 [0.2, 0.4, 0.6, 0.8, 1.0]
    ]

inBetween :: Test
inBetween =
  Test.group "inBetween" $
    [ check T.inBetween 0 []
    , check T.inBetween 1 []
    , check T.inBetween 2 [0.5]
    , check T.inBetween 5 [0.2, 0.4, 0.6, 0.8]
    ]

midpoints :: Test
midpoints =
  Test.group "midpoints" $
    [ check T.midpoints 0 []
    , check T.midpoints 1 [0.5]
    , check T.midpoints 2 [0.25, 0.75]
    , check T.midpoints 5 [0.1, 0.3, 0.5, 0.7, 0.9]
    ]
