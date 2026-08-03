module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polyline2D (data Polyline2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Svg qualified as Svg
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

formatLength :: Length -> Text
formatLength length =
  Text.number (Length.inMeters length) <> "m"

testCurve :: Text -> Curve2D Meters -> IO ()
testCurve label curve = Tolerance.using (Length.meters 1e-12) do
  let length = Curve2D.length curve
  IO.printLine (label <> ": " <> formatLength length)

testLineLength :: IO ()
testLineLength = Tolerance.using (Length.meters 1e-6) do
  testCurve "Line" (Curve2D.lineFrom Point2D.origin (Point2D.centimeters 30.0 40.0))

testQuadraticSplineLength :: IO ()
testQuadraticSplineLength = do
  let p1 = Point2D.origin
  let p2 = Point2D.centimeters 20.0 30.0
  let p3 = Point2D.centimeters 40.0 0.0
  let spline = Curve2D.quadraticBezier p1 p2 p3
  testCurve "Quadratic spline" spline
  IO.printLine ("Analytical value: " <> formatLength (analyticalLength p1 p2 p3))

analyticalLength :: Point2D Meters -> Point2D Meters -> Point2D Meters -> Length
analyticalLength (Point2D x0 y0) (Point2D x1 y1) (Point2D x2 y2) = do
  let ax = x0 - 2.0 * x1 + x2
  let ay = y0 - 2.0 * y1 + y2
  let bx = 2.0 * x1 - 2.0 * x0
  let by = 2.0 * y1 - 2.0 * y0
  let a = 4.0 * (ax * ax + ay * ay)
  let b = 4.0 * (ax * bx + ay * by)
  let c = bx * bx + by * by
  let s_abc = 2.0 * Area.sqrt (a + b + c)
  let a_2 = Area.sqrt a
  let a_32 = 2.0 * a * a_2
  let c_2 = 2.0 * Area.sqrt c
  let ba = b / a_2
  (a_32 * s_abc + a_2 * b * (s_abc - c_2) + (4.0 * c * a - b * b) * Number.log ((2.0 * a_2 + ba + s_abc) / (ba + c_2))) / (4.0 * a_32)

testCubicSplineParameterization :: IO ()
testCubicSplineParameterization = Tolerance.using Length.defaultTolerance do
  let p1 = Point2D.centimeters 5.0 5.0
  let p2 = Point2D.centimeters 7.0 8.0
  let p3 = Point2D.centimeters 16.0 15.0
  let p4 = Point2D.centimeters 25.0 5.0
  let spline = Curve2D.cubicBezier p1 p2 p3 p4
  let reversedSpline = Curve2D.reverse spline
  let length = Curve2D.length spline
  IO.printLine ("Cubic spline: " <> formatLength length)
  let drawCurve fileName curve parameterization = do
        let parameterValues = List.map parameterization (Parameter.steps 30)
        let drawPoint parameterValue = do
              let point = Curve2D.point curve parameterValue
              let diameter = Length.millimeters 3.0
              let startHue = Angle.turns 0.25
              let endHue = Angle.turns 0.75
              let hue = Quantity.interpolateFrom startHue endHue parameterValue
              let fillColor = Color.hsl1 hue 0.5 0.5
              Svg.circleWith [Svg.fillColor fillColor] (Circle2D.withDiameter diameter point)
        let resolution = Resolution.maxError Length.micrometer
        Svg.write fileName (Svg.padding Length.centimeter) $
          Svg.group
            [ Svg.curve resolution curve
            , Svg.combine drawPoint parameterValues
            ]
  drawCurve "executables/arc-length/cubic-spline.svg" spline id
  drawCurve "executables/arc-length/cubic-spline-reversed.svg" reversedSpline id
  drawCurve
    "executables/arc-length/parameterized-spline.svg"
    spline
    (Curve2D.uniformParameterization spline)
  drawCurve
    "executables/arc-length/parameterized-spline-reversed.svg"
    reversedSpline
    (Curve2D.uniformParameterization reversedSpline)
  drawParameterization
    "executables/arc-length/parameterization.svg"
    (Curve2D.uniformParameterization spline)
  drawParameterization
    "executables/arc-length/parameterization-reversed.svg"
    (Curve2D.uniformParameterization reversedSpline)

drawParameterization :: Text -> (Number -> Number) -> IO ()
drawParameterization fileName parameterization = do
  tValues <- Parameter.steps 100 & NonEmpty.fromList & Maybe.orFail "Should not be empty"
  let size = Length.centimeters 10.0
  let point tValue = Point2D (size * tValue) (size * parameterization tValue)
  let points :: NonEmpty (Point2D Meters) = NonEmpty.map point tValues
  let polyline = Polyline2D points
  Svg.write fileName (Svg.padding Length.centimeter) (Svg.polyline polyline)

main :: IO ()
main = do
  testLineLength
  testQuadraticSplineLength
  testCubicSplineParameterization
