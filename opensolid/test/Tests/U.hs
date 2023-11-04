module Tests.U (tests) where

import OpenSolid
import String qualified
import Test (Test)
import Test qualified
import U qualified

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
    [ check U.steps 0 []
    , check U.steps 1 [0.0, 1.0]
    , check U.steps 2 [0.0, 0.5, 1.0]
    , check U.steps 5 [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
    ]

leading :: Test
leading =
  Test.group "leading" $
    [ check U.leading 0 []
    , check U.leading 1 [0.0]
    , check U.leading 2 [0.0, 0.5]
    , check U.leading 5 [0.0, 0.2, 0.4, 0.6, 0.8]
    ]

trailing :: Test
trailing =
  Test.group "trailing" $
    [ check U.trailing 0 []
    , check U.trailing 1 [1.0]
    , check U.trailing 2 [0.5, 1.0]
    , check U.trailing 5 [0.2, 0.4, 0.6, 0.8, 1.0]
    ]

inBetween :: Test
inBetween =
  Test.group "inBetween" $
    [ check U.inBetween 0 []
    , check U.inBetween 1 []
    , check U.inBetween 2 [0.5]
    , check U.inBetween 5 [0.2, 0.4, 0.6, 0.8]
    ]

midpoints :: Test
midpoints =
  Test.group "midpoints" $
    [ check U.midpoints 0 []
    , check U.midpoints 1 [0.5]
    , check U.midpoints 2 [0.25, 0.75]
    , check U.midpoints 5 [0.1, 0.3, 0.5, 0.7, 0.9]
    ]
