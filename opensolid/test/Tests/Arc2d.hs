module Tests.Arc2d (tests) where

import Angle qualified
import Arc2d qualified
import Curve2d qualified
import Float qualified
import OpenSolid
import Parameter qualified
import Point2d qualified
import Qty qualified
import String qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Tolerance qualified
import Units (Meters)

tests :: Tolerance Meters => List Test
tests =
  [ from
  , deformation
  ]

from :: Tolerance Meters => Test
from = do
  let testArcMidpoint numDegrees (expectedX, expectedY) = do
        let label = String.fromInt numDegrees + " degrees"
        let sweptAngle = Angle.degrees (Float.fromInt numDegrees)
        let expectedPoint = Point2d.meters expectedX expectedY
        Test.verify label Test.do
          let arc = Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) sweptAngle
          Test.expect (Curve2d.evaluateAt 0.5 arc ~= expectedPoint)
  let invSqrt2 = 1 / Float.sqrt 2.0
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1 - invSqrt2)
    , testArcMidpoint -90 (1 - invSqrt2, invSqrt2)
    , testArcMidpoint 180 (1.0, 0.0)
    , testArcMidpoint -180 (0.0, 1.0)
    ]

deformation :: Tolerance Meters => Test
deformation = Test.check 100 "deformation" Test.do
  initialArc <- Random.arc2d
  transform <- Random.affineTransform2d
  t <- Parameter.random
  let transformedArc = Curve2d.transformBy transform initialArc
  let startOfTransformed = Curve2d.startPoint transformedArc
  let endOfTransformed = Curve2d.endPoint transformedArc
  let pointOnTransformed = Curve2d.evaluateAt t transformedArc
  let transformOfStart = Point2d.transformBy transform (Curve2d.startPoint initialArc)
  let transformOfEnd = Point2d.transformBy transform (Curve2d.endPoint initialArc)
  let transformOfPoint = Point2d.transformBy transform (Curve2d.evaluateAt t initialArc)
  case transformedArc of
    Curve2d.Arc
      { centerPoint
      , majorDirection
      , minorDirection
      , majorRadius
      , minorRadius
      , startAngle
      , endAngle
      } -> do
        let evaluateEllipticalArc u = do
              let theta = Qty.interpolateFrom startAngle endAngle u
              centerPoint
                + (majorRadius * majorDirection * Angle.cos theta)
                + (minorRadius * minorDirection * Angle.sin theta)
        Test.all
          [ Test.expect (startOfTransformed ~= transformOfStart)
              |> Test.output "startOfTransformed" startOfTransformed
              |> Test.output "transformOfStart" transformOfStart
          , Test.expect (endOfTransformed ~= transformOfEnd)
          , Test.expect (pointOnTransformed ~= transformOfPoint)
          , Test.expect (startOfTransformed ~= evaluateEllipticalArc 0.0)
              |> Test.output "startOfTransformed" startOfTransformed
              |> Test.output "evaluateEllipticalArc 0.0" (evaluateEllipticalArc 0.0)
          , Test.expect (endOfTransformed ~= evaluateEllipticalArc 1.0)
          , Test.expect (pointOnTransformed ~= evaluateEllipticalArc t)
          , Test.expect (Tolerance.using 1e-12 (majorDirection <> minorDirection ~= 0.0))
              |> Test.output "majorDirection" majorDirection
              |> Test.output "minorDirection" minorDirection
              |> Test.output "major/minor dot product" (majorDirection <> minorDirection)
          ]
    curve ->
      Test.fail "Expected an elliptical arc"
        |> Test.output "Actual curve" curve
