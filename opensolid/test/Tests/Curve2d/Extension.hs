module Tests.Curve2d.Extension (tests) where

import Angle qualified
import Arc2d qualified
import Curve2d qualified
import Curve2d.Extension qualified
import Float qualified
import Length qualified
import List qualified
import OpenSolid
import Parameter qualified
import Qty qualified
import Test (Expectation, Test)
import Test qualified
import Tests.Curve2d qualified
import Tests.Random as Random
import Tolerance qualified
import Units (Meters)
import Vector2d (Vector2d)
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
  arcEndAngle <- Qty.random -Angle.pi Angle.pi
  let arcSweptAngle = arcEndAngle - arcStartAngle
  t <- Parameter.random

  let mainCurve = Arc2d.polar arcCenter arcRadius arcStartAngle arcEndAngle
  let mainCurveFirstDerivative = Curve2d.derivative mainCurve
  let mainCurveSecondDerivative = VectorCurve2d.derivative mainCurveFirstDerivative
  let mainCurveThirdDerivative = VectorCurve2d.derivative mainCurveSecondDerivative

  let extensionScale = 1e-3

  -- Start extension
  let startExtensionStartAngle = arcStartAngle - extensionScale * arcSweptAngle
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
  let startExtensionDerivativePair extensionDerivative mainDerivative =
        ( VectorCurve2d.evaluateAt 1.0 extensionDerivative
        , VectorCurve2d.evaluateAt 0.0 mainDerivative
        )

  -- End extension
  let endExtensionEndAngle = arcEndAngle + extensionScale * arcSweptAngle
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
  let endExtensionDerivativePair mainDerivative extensionDerivative =
        ( VectorCurve2d.evaluateAt 1.0 mainDerivative
        , VectorCurve2d.evaluateAt 0.0 extensionDerivative
        )

  Test.all
    [ Test.all
        [ Tests.Curve2d.firstDerivativeIsConsistent startExtensionCurve t
        , Tests.Curve2d.secondDerivativeIsConsistent startExtensionCurve t
        , derivativesAreConsistent
            [ startExtensionDerivativePair startExtensionCurveFirstDerivative mainCurveFirstDerivative
            , startExtensionDerivativePair startExtensionCurveSecondDerivative mainCurveSecondDerivative
            , startExtensionDerivativePair startExtensionCurveThirdDerivative mainCurveThirdDerivative
            ]
        ]
    , Test.all
        [ Tests.Curve2d.firstDerivativeIsConsistent endExtensionCurve t
        , Tests.Curve2d.secondDerivativeIsConsistent endExtensionCurve t
        , derivativesAreConsistent
            [ endExtensionDerivativePair mainCurveFirstDerivative endExtensionCurveFirstDerivative
            , endExtensionDerivativePair mainCurveSecondDerivative endExtensionCurveSecondDerivative
            , endExtensionDerivativePair mainCurveThirdDerivative endExtensionCurveThirdDerivative
            ]
        ]
    ]

derivativesAreConsistent ::
  Tolerance Meters =>
  List (Vector2d (space @ Meters), Vector2d (space @ Meters)) ->
  Expectation
derivativesAreConsistent derivativePairs =
  case derivativePairs of
    [] -> Test.pass
    ((first1, first2) : _) -> do
      let ratio1 = Length.meter / Vector2d.magnitude first1
      let ratio2 = Length.meter / Vector2d.magnitude first2
      let derivativePairIsConsistent i (v1, v2) = do
            let n = i + 1
            let scale1 = ratio1 ** n
            let scale2 = ratio2 ** n
            let normalized1 = scale1 * v1
            let normalized2 = scale2 * v2
            let error = normalized2 - normalized1
            let scaledTolerance = Tolerance.times (Float.max scale1 scale2)
            Test.expect (Tolerance.using scaledTolerance (error ~= Vector2d.zero))
              |> Test.output "n" n
              |> Test.output "v1" v1
              |> Test.output "v2" v2
              |> Test.output "scale1" scale1
              |> Test.output "scale2" scale2
              |> Test.output "normalized1" normalized1
              |> Test.output "normalized2" normalized2
              |> Test.output "error" error
              |> Test.output "error magnitude" (Vector2d.magnitude error)
      Test.all (List.mapWithIndex derivativePairIsConsistent derivativePairs)
