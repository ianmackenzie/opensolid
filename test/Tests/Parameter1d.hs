module Tests.Parameter1d (tests) where

import Expect qualified
import OpenSolid
import Parameter1d qualified
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

steps :: Test
steps =
  Test.group "steps" $
    [ Test.verify "0" (Parameter1d.steps 0 identity |> Expect.equal [])
    , Test.verify "1" (Parameter1d.steps 1 identity |> Expect.equal [0.0, 1.0])
    , Test.verify "2" (Parameter1d.steps 2 identity |> Expect.equal [0.0, 0.5, 1.0])
    , Test.verify "5" (Parameter1d.steps 5 identity |> Expect.equal [0.0, 0.2, 0.4, 0.6, 0.8, 1.0])
    ]

leading :: Test
leading =
  Test.group "leading" $
    [ Test.verify "0" (Parameter1d.leading 0 identity |> Expect.equal [])
    , Test.verify "1" (Parameter1d.leading 1 identity |> Expect.equal [0.0])
    , Test.verify "2" (Parameter1d.leading 2 identity |> Expect.equal [0.0, 0.5])
    , Test.verify "5" (Parameter1d.leading 5 identity |> Expect.equal [0.0, 0.2, 0.4, 0.6, 0.8])
    ]

trailing :: Test
trailing =
  Test.group "trailing" $
    [ Test.verify "0" (Parameter1d.trailing 0 identity |> Expect.equal [])
    , Test.verify "1" (Parameter1d.trailing 1 identity |> Expect.equal [1.0])
    , Test.verify "2" (Parameter1d.trailing 2 identity |> Expect.equal [0.5, 1.0])
    , Test.verify "5" (Parameter1d.trailing 5 identity |> Expect.equal [0.2, 0.4, 0.6, 0.8, 1.0])
    ]

inBetween :: Test
inBetween =
  Test.group "inBetween" $
    [ Test.verify "0" (Parameter1d.inBetween 0 identity |> Expect.equal [])
    , Test.verify "1" (Parameter1d.inBetween 1 identity |> Expect.equal [])
    , Test.verify "2" (Parameter1d.inBetween 2 identity |> Expect.equal [0.5])
    , Test.verify "5" (Parameter1d.inBetween 5 identity |> Expect.equal [0.2, 0.4, 0.6, 0.8])
    ]

midpoints :: Test
midpoints =
  Test.group "midpoints" $
    [ Test.verify "0" (Parameter1d.midpoints 0 identity |> Expect.equal [])
    , Test.verify "1" (Parameter1d.midpoints 1 identity |> Expect.equal [0.5])
    , Test.verify "2" (Parameter1d.midpoints 2 identity |> Expect.equal [0.25, 0.75])
    , Test.verify "5" (Parameter1d.midpoints 5 identity |> Expect.equal [0.1, 0.3, 0.5, 0.7, 0.9])
    ]
