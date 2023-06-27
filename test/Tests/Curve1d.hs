module Tests.Curve1d (tests) where

import Angle qualified
import Curve1d ()
import Curve1d qualified
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
import Expect (Expectation)
import Expect qualified
import OpenSolid
import Test (Test)
import Test qualified
import Units (Unitless)

tests :: List Test
tests =
  let ?tolerance = 1e-12
   in [ crossingRoots
      , tangentRoots
      , approximateEquality
      ]

expectRoot :: Tolerance Unitless => Root -> Root -> Expectation
expectRoot expected actual
  | expected.order /= actual.order = Expect.fail "Root orders do not match"
  | expected.sign /= actual.sign = Expect.fail "Root signs do not match"
  | expected.value != actual.value = Expect.fail "Root values do not match"
  | otherwise = Expect.pass

expectRoots :: Tolerance Unitless => List Root -> List Root -> Expectation
expectRoots = Expect.list expectRoot

crossingRoots :: Tolerance Unitless => Test
crossingRoots = Test.verify "Crossing roots" $ do
  let t = Curve1d.parameter
  let x = 3.0 * t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  roots <- Curve1d.roots y
  roots
    |> expectRoots
      [ Root 0.0 0 Positive
      , Root (1 / 3) 0 Negative
      , Root (2 / 3) 0 Positive
      ]

tangentRoots :: Tolerance Unitless => Test
tangentRoots = Test.verify "Tangent roots" $ do
  let t = Curve1d.parameter
  let theta = Angle.fullTurn * t
  let expression = Curve1d.squared (Curve1d.sin theta)
  roots <- Curve1d.roots expression
  roots
    |> expectRoots
      [ Root 0.0 1 Positive
      , Root 0.5 1 Positive
      , Root 1.0 1 Positive
      ]

approximateEquality :: Tolerance Unitless => Test
approximateEquality =
  let t = Curve1d.parameter
      theta = Angle.radian * t
      sinTheta = Curve1d.sin theta
      cosTheta = Curve1d.cos theta
      sumOfSquares = Curve1d.squared sinTheta + Curve1d.squared cosTheta
   in Test.group "Approximate equality" $
        [ Test.verify "sin(x) != cos(x)" $
            Expect.expect (sinTheta != cosTheta) "sin(x) should not equal cos(x)"
        , Test.verify "sin(x) ~= cos(pi/2 - x)" $
            (sinTheta |> Expect.approximately (Curve1d.cos (Angle.degrees 90.0 - theta)))
        , Test.verify "sin^2(x) + cos^2(x) ~= 1.0" $
            (sumOfSquares |> Expect.approximately 1.0)
        , Test.verify "sin^2(x) + cos^2(x) != 2.0" $
            Expect.expect (sumOfSquares != 2.0) "sin^2(x) + cos^2(x) should not equal 2.0"
        ]
