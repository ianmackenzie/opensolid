module Tests.Parameter1d (tests) where

import OpenSolid
import Parameter1d qualified
import Test (Test)
import Test qualified
import Text qualified

tests :: List Test
tests =
  [ steps
  , leading
  , trailing
  , inBetween
  , midpoints
  ]

check :: (Int -> (Float -> Float) -> List Float) -> Int -> List Float -> Test
check function n expected =
  Test.verify (Text.fromInt n) $
    Test.expect (function n identity == expected)

steps :: Test
steps =
  Test.group "steps" $
    [ check Parameter1d.steps 0 []
    , check Parameter1d.steps 1 [0.0, 1.0]
    , check Parameter1d.steps 2 [0.0, 0.5, 1.0]
    , check Parameter1d.steps 5 [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
    ]

leading :: Test
leading =
  Test.group "leading" $
    [ check Parameter1d.leading 0 []
    , check Parameter1d.leading 1 [0.0]
    , check Parameter1d.leading 2 [0.0, 0.5]
    , check Parameter1d.leading 5 [0.0, 0.2, 0.4, 0.6, 0.8]
    ]

trailing :: Test
trailing =
  Test.group "trailing" $
    [ check Parameter1d.trailing 0 []
    , check Parameter1d.trailing 1 [1.0]
    , check Parameter1d.trailing 2 [0.5, 1.0]
    , check Parameter1d.trailing 5 [0.2, 0.4, 0.6, 0.8, 1.0]
    ]

inBetween :: Test
inBetween =
  Test.group "inBetween" $
    [ check Parameter1d.inBetween 0 []
    , check Parameter1d.inBetween 1 []
    , check Parameter1d.inBetween 2 [0.5]
    , check Parameter1d.inBetween 5 [0.2, 0.4, 0.6, 0.8]
    ]

midpoints :: Test
midpoints =
  Test.group "midpoints" $
    [ check Parameter1d.midpoints 0 []
    , check Parameter1d.midpoints 1 [0.5]
    , check Parameter1d.midpoints 2 [0.25, 0.75]
    , check Parameter1d.midpoints 5 [0.1, 0.3, 0.5, 0.7, 0.9]
    ]
