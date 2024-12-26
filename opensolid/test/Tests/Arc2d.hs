module Tests.Arc2d (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2d qualified as Arc2d
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import OpenSolid.Text qualified as Text
import OpenSolid.Units (Meters)

tests :: Tolerance Meters => List Test
tests =
  [ from
  , deformation
  ]

from :: Tolerance Meters => Test
from = do
  let testArcMidpoint numDegrees (expectedX, expectedY) = do
        let label = Text.int numDegrees + " degrees"
        let sweptAngle = Angle.degrees (Float.int numDegrees)
        let expectedPoint = Point2d.meters expectedX expectedY
        Test.verify label Test.do
          let arc = Arc2d.from Point2d.origin (Point2d.meters 1.0 1.0) sweptAngle
          Test.expect (Curve2d.evaluate arc 0.5 ~= expectedPoint)
  let invSqrt2 = 1.0 / Float.sqrt 2.0
  Test.group "from" $
    [ testArcMidpoint 90 (invSqrt2, 1.0 - invSqrt2)
    , testArcMidpoint -90 (1.0 - invSqrt2, invSqrt2)
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
  let pointOnTransformed = Curve2d.evaluate transformedArc t
  let transformOfStart = Point2d.transformBy transform (Curve2d.startPoint initialArc)
  let transformOfEnd = Point2d.transformBy transform (Curve2d.endPoint initialArc)
  let transformOfPoint = Point2d.transformBy transform (Curve2d.evaluate initialArc t)
  Test.all
    [ Test.expect (startOfTransformed ~= transformOfStart)
        |> Test.output "startOfTransformed" startOfTransformed
        |> Test.output "transformOfStart" transformOfStart
    , Test.expect (endOfTransformed ~= transformOfEnd)
    , Test.expect (pointOnTransformed ~= transformOfPoint)
    ]
