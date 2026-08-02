module Tests.Curve1D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Root (Root (Root))
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified
import Tests.Matching (matching)

tests :: List Test
tests =
  [ crossingRoots
  , tangentRoots
  , approximateEquality
  ]

crossingRoots :: Test
crossingRoots = Test.verifyWith Tolerance.unitless "crossingRoots" do
  let x = 3.0 * Curve1D.t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let expectedRoots = [Root 0.0 0 Positive, Root (1 / 3) 0 Negative, Root (2 / 3) 0 Positive]
  roots <- Curve1D.roots y ?? fail
  Test.expect (matching roots expectedRoots)
    & Test.output "roots" roots
    & Test.output "expectedRoots" expectedRoots

tangentRoots :: Test
tangentRoots = Test.verifyWith Tolerance.unitless "tangentRoots" do
  let theta = Angle.twoPi * Curve1D.t
  let expression = Curve1D.squared (Curve1D.sin theta)
  let expectedRoots = [Root t 1 Positive | t <- [0.0, 0.5, 1.0]]
  roots <- Curve1D.roots expression ?? fail
  Test.expect (matching roots expectedRoots)
    & Test.output "roots" roots
    & Test.output "expectedRoots" expectedRoots

approximateEquality :: Test
approximateEquality = Test.verifyWith Tolerance.unitless "approximateEquality" do
  let theta = Angle.twoPi * Curve1D.t
  let sinTheta = Curve1D.sin theta
  let cosTheta = Curve1D.cos theta
  let sumOfSquares = Curve1D.squared sinTheta + Curve1D.squared cosTheta
  Test.all
    [ Test.expect (not $ sinTheta ~= cosTheta)
    , Test.expect (sinTheta ~= Curve1D.cos (Angle.degrees 90.0 - theta))
    , Test.expect (sumOfSquares ~= Curve1D.constant 1.0)
    , Test.expect (not $ sumOfSquares ~= Curve1D.constant 2.0)
    ]
