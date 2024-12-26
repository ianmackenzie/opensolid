module Tests.Curve1d (tests) where

import Angle qualified
import OpenSolid.Prelude
import OpenSolid.Curve1d ()
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve1d.Zero (Zero (Zero))
import Test (Test)
import Test qualified
import Tolerance qualified

tests :: List Test
tests =
  Tolerance.using 1e-12 $
    [ crossingZeros
    , tangentZeros
    , approximateEquality
    ]

crossingZeros :: Tolerance Unitless => Test
crossingZeros = Test.verify "crossingZeros" Test.do
  let x = 3.0 * Curve1d.t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let expectedZeros = [Zero 0.0 0 Positive, Zero (1 / 3) 0 Negative, Zero (2 / 3) 0 Positive]
  zeros <- Curve1d.zeros y
  Test.expect (zeros ~= expectedZeros)
    |> Test.output "zeros" zeros
    |> Test.output "expectedZeros" expectedZeros

tangentZeros :: Tolerance Unitless => Test
tangentZeros = Test.verify "tangentZeros" Test.do
  let theta = Angle.twoPi * Curve1d.t
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expectedZeros = [Zero t 1 Positive | t <- [0.0, 0.5, 1.0]]
  zeros <- Curve1d.zeros expression
  Test.expect (zeros ~= expectedZeros)
    |> Test.output "zeros" zeros
    |> Test.output "expectedZeros" expectedZeros

approximateEquality :: Tolerance Unitless => Test
approximateEquality = Test.verify "approximateEquality" Test.do
  let theta = Angle.twoPi * Curve1d.t
  let sinTheta = Curve1d.sin theta
  let cosTheta = Curve1d.cos theta
  let sumOfSquares = Curve1d.squared sinTheta + Curve1d.squared cosTheta
  Test.all
    [ Test.expect (sinTheta != cosTheta)
    , Test.expect (sinTheta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
    , Test.expect (sumOfSquares ~= 1.0)
    , Test.expect (sumOfSquares != 2.0)
    ]
