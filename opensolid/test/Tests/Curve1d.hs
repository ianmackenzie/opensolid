module Tests.Curve1d (tests) where

import Angle qualified
import Curve1d ()
import Curve1d qualified
import Curve1d.Root (Root (Root))
import OpenSolid
import Test (Test)
import Test qualified
import Tolerance qualified

tests :: List Test
tests =
  Tolerance.using 1e-12 $
    [ crossingRoots
    , tangentRoots
    , approximateEquality
    ]

crossingRoots :: Tolerance Unitless => Test
crossingRoots = Test.verify "Crossing roots" Test.do
  let x = 3.0 * Curve1d.t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let expectedRoots = [Root 0.0 0 Positive, Root (1 / 3) 0 Negative, Root (2 / 3) 0 Positive]
  case Curve1d.zeros y of
    Curve1d.ZeroEverywhere -> Test.fail "Curve incorrectly reported as identically zero"
    Curve1d.Zeros roots -> Test.expect (roots ~= expectedRoots)

tangentRoots :: Tolerance Unitless => Test
tangentRoots = Test.verify "Tangent roots" Test.do
  let theta = Angle.twoPi * Curve1d.t
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expectedRoots = [Root t 1 Positive | t <- [0.0, 0.5, 1.0]]
  case Curve1d.zeros expression of
    Curve1d.ZeroEverywhere -> Test.fail "Curve incorrectly reported as identically zero"
    Curve1d.Zeros roots -> Test.expect (roots ~= expectedRoots)

approximateEquality :: Tolerance Unitless => Test
approximateEquality = do
  let theta = Angle.twoPi * Curve1d.t
  let sinTheta = Curve1d.sin theta
  let cosTheta = Curve1d.cos theta
  let sumOfSquares = Curve1d.squared sinTheta + Curve1d.squared cosTheta
  Test.group "Approximate equality" $
    [ Test.verify "sin(x) != cos(x)" $
        Test.expect (sinTheta != cosTheta)
    , Test.verify "sin(x) ~= cos(pi/2 - x)" $
        Test.expect (sinTheta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
    , Test.verify "sin^2(x) + cos^2(x) ~= 1.0" $
        Test.expect (sumOfSquares ~= 1.0)
    , Test.verify "sin^2(x) + cos^2(x) != 2.0" $
        Test.expect (sumOfSquares != 2.0)
    ]
