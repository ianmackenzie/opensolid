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
crossingRoots = Test.verify "crossingRoots" Test.do
  let x = 3 * Curve1d.parameter
  let y = (x - 1) * (x - 1) * (x - 1) - (x - 1)
  let expectedRoots = [Root 0.0 0 Positive, Root (1 / 3) 0 Negative, Root (2 / 3) 0 Positive]
  roots <- Curve1d.zeros y
  Test.expect (roots ~= expectedRoots)

tangentRoots :: Tolerance Unitless => Test
tangentRoots = Test.verify "tangentRoots" Test.do
  let theta = Angle.twoPi * Curve1d.parameter
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expectedRoots = [Root t 1 Positive | t <- [0.0, 0.5, 1.0]]
  roots <- Curve1d.zeros expression
  Test.expect (roots ~= expectedRoots)
    |> Test.output "roots" roots
    |> Test.output "expectedRoots" expectedRoots

approximateEquality :: Tolerance Unitless => Test
approximateEquality = Test.verify "approximateEquality" Test.do
  let theta = Angle.twoPi * Curve1d.parameter
  let sinTheta = Curve1d.sin theta
  let cosTheta = Curve1d.cos theta
  let sumOfSquares = Curve1d.squared sinTheta + Curve1d.squared cosTheta
  Test.all
    [ Test.expect (sinTheta != cosTheta)
    , Test.expect (sinTheta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
    , Test.expect (sumOfSquares ~= 1.0)
    , Test.expect (sumOfSquares != 2.0)
    ]
