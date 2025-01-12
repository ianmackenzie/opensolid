module Tests.Curve (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero (Zero (Zero))
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  Tolerance.using 1e-12 $
    [ crossingZeros
    , tangentZeros
    , approximateEquality
    ]

crossingZeros :: Tolerance Unitless => Test
crossingZeros = Test.verify "crossingZeros" Test.do
  let x = 3.0 * Curve.t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let expectedZeros = [Zero 0.0 0 Positive, Zero (1 / 3) 0 Negative, Zero (2 / 3) 0 Positive]
  zeros <- Curve.zeros y
  Test.expect (zeros ~= expectedZeros)
    |> Test.output "zeros" zeros
    |> Test.output "expectedZeros" expectedZeros

tangentZeros :: Tolerance Unitless => Test
tangentZeros = Test.verify "tangentZeros" Test.do
  let theta = Angle.twoPi * Curve.t
  let expression = Curve.squared (Curve.sin theta)
  let expectedZeros = [Zero t 1 Positive | t <- [0.0, 0.5, 1.0]]
  zeros <- Curve.zeros expression
  Test.expect (zeros ~= expectedZeros)
    |> Test.output "zeros" zeros
    |> Test.output "expectedZeros" expectedZeros

approximateEquality :: Tolerance Unitless => Test
approximateEquality = Test.verify "approximateEquality" Test.do
  let theta = Angle.twoPi * Curve.t
  let sinTheta = Curve.sin theta
  let cosTheta = Curve.cos theta
  let sumOfSquares = Curve.squared sinTheta + Curve.squared cosTheta
  Test.all
    [ Test.expect (sinTheta != cosTheta)
    , Test.expect (sinTheta ~= Curve.cos (Angle.degrees 90.0 - theta))
    , Test.expect (sumOfSquares ~= 1.0)
    , Test.expect (sumOfSquares != 2.0)
    ]
