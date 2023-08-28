module Tests.Curve1d (tests) where

import Angle qualified
import Curve1d ()
import Curve1d qualified
import Curve1d.Root (Root (Root))
import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  let ?tolerance = 1e-12
   in [ crossingRoots
      , tangentRoots
      , approximateEquality
      ]

crossingRoots :: Tolerance Unitless => Test
crossingRoots = Test.verify "Crossing roots" $ do
  let t = Curve1d.parameter
  let x = 3.0 * t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  roots <- Curve1d.roots y
  let expectedRoots =
        [ Root 0.0 0 Positive
        , Root (1 / 3) 0 Negative
        , Root (2 / 3) 0 Positive
        ]
  Test.expect (roots ~= expectedRoots)

tangentRoots :: Tolerance Unitless => Test
tangentRoots = Test.verify "Tangent roots" $ do
  let t = Curve1d.parameter
  let theta = Angle.fullTurn * t
  let expression = Curve1d.squared (Curve1d.sin theta)
  roots <- Curve1d.roots expression
  let expectedRoots =
        [ Root 0.0 1 Positive
        , Root 0.5 1 Positive
        , Root 1.0 1 Positive
        ]
  Test.expect (roots ~= expectedRoots)

approximateEquality :: Tolerance Unitless => Test
approximateEquality =
  let t = Curve1d.parameter
      theta = Angle.radian * t
      sinTheta = Curve1d.sin theta
      cosTheta = Curve1d.cos theta
      sumOfSquares = Curve1d.squared sinTheta + Curve1d.squared cosTheta
   in Test.group "Approximate equality" $
        [ Test.verify "sin(x) != cos(x)" $
            Test.expect (sinTheta != cosTheta)
        , Test.verify "sin(x) ~= cos(pi/2 - x)" $
            Test.expect (sinTheta ~= Curve1d.cos (Angle.degrees 90.0 - theta))
        , Test.verify "sin^2(x) + cos^2(x) ~= 1.0" $
            Test.expect (sumOfSquares ~= 1.0)
        , Test.verify "sin^2(x) + cos^2(x) != 2.0" $
            Test.expect (sumOfSquares != 2.0)
        ]
