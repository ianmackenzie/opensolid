module Tests.Bezier (tests) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Unboxed.Math
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ cubicHermite
  , quinticHermite
  , unboxedQuinticHermiteEvaluation
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

unboxedQuinticHermiteEvaluation :: Test
unboxedQuinticHermiteEvaluation = Test.check 100 "unboxedQuinticHermiteEvaluation" do
  let randomNumber = Random.number -10.0 10.0
  startPoint <- Test.generate randomNumber
  startFirstDerivative <- Test.generate randomNumber
  startSecondDerivative <- Test.generate randomNumber
  endPoint <- Test.generate randomNumber
  endFirstDerivative <- Test.generate randomNumber
  endSecondDerivative <- Test.generate randomNumber
  let !(Quantity# p1#, Quantity# p2#, Quantity# p3#, Quantity# p4#, Quantity# p5#, Quantity# p6#) =
        Bezier.quinticHermite
          startPoint
          startFirstDerivative
          startSecondDerivative
          endPoint
          endFirstDerivative
          endSecondDerivative
  let curve =
        Curve1D.hermite
          startPoint
          [startFirstDerivative, startSecondDerivative]
          endPoint
          [endFirstDerivative, endSecondDerivative]
  t <- Test.generate Parameter.random
  let !(Quantity# t#) = t
  let actual = Quantity# (quinticBezier# p1# p2# p3# p4# p5# p6# t#)
  let expected = Curve1D.value curve t
  Test.expect (Tolerance.using Tolerance.unitless (actual ~= expected))
    & Test.output "actual" actual
    & Test.output "expected" expected
