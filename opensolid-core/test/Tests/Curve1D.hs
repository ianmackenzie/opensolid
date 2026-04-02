module Tests.Curve1D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero (Zero (Zero))
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ crossingZeros
  , tangentZeros
  , approximateEquality
  ]

crossingZeros :: Test
crossingZeros = Test.verifyWith Tolerance.unitless "crossingZeros" do
  let x = 3.0 * Curve1D.t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let expectedZeros = [Zero 0.0 0 Positive, Zero (1 / 3) 0 Negative, Zero (2 / 3) 0 Positive]
  zeros <- Result.orFail (Curve1D.zeros y)
  Test.expect (zeros ~= expectedZeros)
    & Test.output "zeros" zeros
    & Test.output "expectedZeros" expectedZeros

tangentZeros :: Test
tangentZeros = Test.verifyWith Tolerance.unitless "tangentZeros" do
  let theta = Angle.twoPi * Curve1D.t
  let expression = Curve1D.squared (Curve1D.sin theta)
  let expectedZeros = [Zero t 1 Positive | t <- [0.0, 0.5, 1.0]]
  zeros <- Result.orFail (Curve1D.zeros expression)
  Test.expect (zeros ~= expectedZeros)
    & Test.output "zeros" zeros
    & Test.output "expectedZeros" expectedZeros

approximateEquality :: Test
approximateEquality = Test.verifyWith Tolerance.unitless "approximateEquality" do
  let theta = Angle.twoPi * Curve1D.t
  let sinTheta = Curve1D.sin theta
  let cosTheta = Curve1D.cos theta
  let sumOfSquares = Curve1D.squared sinTheta + Curve1D.squared cosTheta
  Test.all
    [ Test.expect (sinTheta != cosTheta)
    , Test.expect (sinTheta ~= Curve1D.cos (Angle.degrees 90.0 - theta))
    , Test.expect (sumOfSquares ~= Curve1D.constant 1.0)
    , Test.expect (sumOfSquares != Curve1D.constant 2.0)
    ]
