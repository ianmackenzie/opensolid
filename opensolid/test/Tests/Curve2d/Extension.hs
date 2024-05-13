module Tests.Curve2d.Extension (tests) where

import Angle qualified
import Arc2d qualified
import Curve2d qualified
import Curve2d.Extension qualified
import Length qualified
import OpenSolid
import Parameter qualified
import Point2d qualified
import Qty qualified
import Random qualified
import Sign qualified
import Test (Test)
import Test qualified
import Tests.Curve2d qualified
import Tests.Random as Random
import Tolerance qualified
import Units (Meters)
import Vector2d qualified
import VectorCurve2d qualified

tests :: Tolerance Meters => List Test
tests =
  [ arc
  ]

arc :: Tolerance Meters => Test
arc = Test.check 100 "arc" Test.do
  arcCenter <- Random.point2d
  arcRadius <- Qty.random (Length.meters 0.1) (Length.meters 10.0)
  arcStartAngle <- Qty.random -Angle.pi Angle.pi
  arcSweptAngle <- Random.map (Angle.degree *) Sign.random
  let arcEndAngle = arcStartAngle + arcSweptAngle
  t <- Parameter.random

  let mainCurve = Arc2d.polar arcCenter arcRadius arcStartAngle arcEndAngle
  let mainCurveFirstDerivative = Curve2d.derivative mainCurve
  let mainCurveSecondDerivative = VectorCurve2d.derivative mainCurveFirstDerivative
  let mainCurveThirdDerivative = VectorCurve2d.derivative mainCurveSecondDerivative

  -- Start extension
  let startExtensionStartAngle = arcStartAngle - arcSweptAngle
  let startExtensionArc = Arc2d.polar arcCenter arcRadius startExtensionStartAngle arcStartAngle
  let startExtensionArcFirstDerivative = Curve2d.derivative startExtensionArc
  let startExtensionArcSecondDerivative = VectorCurve2d.derivative startExtensionArcFirstDerivative
  let startExtensionStartDerivatives =
        [ VectorCurve2d.evaluateAt 0.0 startExtensionArcFirstDerivative
        , VectorCurve2d.evaluateAt 0.0 startExtensionArcSecondDerivative
        ]
  let startExtensionCurve =
        Curve2d.Extension.start
          (Curve2d.startPoint startExtensionArc, startExtensionStartDerivatives)
          (mainCurve, 2)
  let startExtensionCurveFirstDerivative = Curve2d.derivative startExtensionCurve
  let startExtensionCurveSecondDerivative = VectorCurve2d.derivative startExtensionCurveFirstDerivative
  let startExtensionCurveThirdDerivative = VectorCurve2d.derivative startExtensionCurveSecondDerivative

  -- End extension
  let endExtensionEndAngle = arcEndAngle + arcSweptAngle
  let endExtensionArc = Arc2d.polar arcCenter arcRadius arcEndAngle endExtensionEndAngle
  let endExtensionArcFirstDerivative = Curve2d.derivative endExtensionArc
  let endExtensionArcSecondDerivative = VectorCurve2d.derivative endExtensionArcFirstDerivative
  let endExtensionEndDerivatives =
        [ VectorCurve2d.evaluateAt 1.0 endExtensionArcFirstDerivative
        , VectorCurve2d.evaluateAt 1.0 endExtensionArcSecondDerivative
        ]
  let endExtensionCurve =
        Curve2d.Extension.end
          (mainCurve, 2)
          (Curve2d.endPoint endExtensionArc, endExtensionEndDerivatives)
  let endExtensionCurveFirstDerivative = Curve2d.derivative endExtensionCurve
  let endExtensionCurveSecondDerivative = VectorCurve2d.derivative endExtensionCurveFirstDerivative
  let endExtensionCurveThirdDerivative = VectorCurve2d.derivative endExtensionCurveSecondDerivative
  let expectEqualPoints analyticCurve extensionCurve = do
        let analyticPoint = Curve2d.pointOn analyticCurve t
        let extensionPoint = Curve2d.pointOn extensionCurve t
        Tolerance.using (Length.meters 1e-14) (Test.expect (analyticPoint ~= extensionPoint))
          |> Test.output "point distance" (Point2d.distanceFrom analyticPoint extensionPoint)
  let expectEqualDerivatives n firstCurve secondCurve = do
        let firstEndValue = VectorCurve2d.evaluateAt 1.0 firstCurve
        let secondStartValue = VectorCurve2d.evaluateAt 0.0 secondCurve
        Test.expect (firstEndValue ~= secondStartValue)
          |> Test.output "Derivative order" n
          |> Test.output "Derivative at end of first curve" firstEndValue
          |> Test.output "Derivative at start of second curve" secondStartValue
          |> Test.output "Derivative difference magnitude" (Vector2d.magnitude (firstEndValue - secondStartValue))

  Test.all
    [ Test.all
        [ expectEqualPoints startExtensionArc startExtensionCurve
        , Tests.Curve2d.firstDerivativeIsConsistent startExtensionCurve t
        , Tests.Curve2d.secondDerivativeIsConsistent startExtensionCurve t
        , expectEqualDerivatives 1 startExtensionCurveFirstDerivative mainCurveFirstDerivative
        , expectEqualDerivatives 2 startExtensionCurveSecondDerivative mainCurveSecondDerivative
        , expectEqualDerivatives 3 startExtensionCurveThirdDerivative mainCurveThirdDerivative
        ]
    , Test.all
        [ expectEqualPoints endExtensionArc endExtensionCurve
        , Tests.Curve2d.firstDerivativeIsConsistent endExtensionCurve t
        , Tests.Curve2d.secondDerivativeIsConsistent endExtensionCurve t
        , expectEqualDerivatives 1 mainCurveFirstDerivative endExtensionCurveFirstDerivative
        , expectEqualDerivatives 2 mainCurveSecondDerivative endExtensionCurveSecondDerivative
        , expectEqualDerivatives 3 mainCurveThirdDerivative endExtensionCurveThirdDerivative
        ]
    ]
