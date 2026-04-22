module Tests.Bezier (tests) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ cubicHermite
  , quinticHermite
  ]

cubicHermite :: Test
cubicHermite = Test.check 100 "cubicHermite" do
  startPoint <- Test.generate Random.point2D
  startFirstDerivative <- Test.generate Random.vector2D
  endPoint <- Test.generate Random.point2D
  endFirstDerivative <- Test.generate Random.vector2D
  let (p1, p2, p3, p4) =
        Bezier.cubicHermite startPoint startFirstDerivative endPoint endFirstDerivative
  let actual = NonEmpty.four p1 p2 p3 p4
  let expected =
        Bezier.hermite startPoint [startFirstDerivative] endPoint [endFirstDerivative]
  Test.expect (actual ~= expected)
    & Test.output "actual" actual
    & Test.output "expected" expected

quinticHermite :: Test
quinticHermite = Test.check 100 "quinticHermite" do
  startPoint <- Test.generate Random.point2D
  startFirstDerivative <- Test.generate Random.vector2D
  startSecondDerivative <- Test.generate Random.vector2D
  endPoint <- Test.generate Random.point2D
  endFirstDerivative <- Test.generate Random.vector2D
  endSecondDerivative <- Test.generate Random.vector2D
  let (p1, p2, p3, p4, p5, p6) =
        Bezier.quinticHermite
          startPoint
          startFirstDerivative
          startSecondDerivative
          endPoint
          endFirstDerivative
          endSecondDerivative
  let actual = NonEmpty.six p1 p2 p3 p4 p5 p6
  let expected =
        Bezier.hermite
          startPoint
          [startFirstDerivative, startSecondDerivative]
          endPoint
          [endFirstDerivative, endSecondDerivative]
  Test.expect (actual ~= expected)
    & Test.output "actual" actual
    & Test.output "expected" expected
