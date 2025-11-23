module Main (main) where

import OpenSolid.Area qualified as Area
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import Prelude hiding (length)

data Space

formatLength :: Length -> Text
formatLength length =
  Text.number (Length.inMeters length) <> "m"

testCurve :: Text -> Curve2d Space Meters -> IO ()
testCurve label curve = Tolerance.using (Length.meters 1e-12) do
  let (_, length) = Curve2d.arcLengthParameterization curve
  IO.printLine (label <> ": " <> formatLength length)

testLineLength :: IO ()
testLineLength = Tolerance.using (Length.meters 1e-6) do
  testCurve "Line" (Curve2d.line Point2d.origin (Point2d.centimeters 30 40))

testQuadraticSplineLength :: IO ()
testQuadraticSplineLength = do
  let p1 = Point2d.origin
  let p2 = Point2d.centimeters 20 30
  let p3 = Point2d.centimeters 40 0
  let spline = Curve2d.quadraticBezier p1 p2 p3
  testCurve "Quadratic spline" spline
  IO.printLine ("Analytical value: " <> formatLength (analyticalLength p1 p2 p3))

analyticalLength ::
  Point2d space Meters ->
  Point2d space Meters ->
  Point2d space Meters ->
  Length
analyticalLength (Point2d x0 y0) (Point2d x1 y1) (Point2d x2 y2) = do
  let ax = x0 .-. 2 *. x1 .+. x2
  let ay = y0 .-. 2 *. y1 .+. y2
  let bx = 2 *. x1 .-. 2 *. x0
  let by = 2 *. y1 .-. 2 *. y0
  let a = 4 *. (ax .*. ax .+. ay .*. ay)
  let b = 4 *. (ax .*. bx .+. ay .*. by)
  let c = bx .*. bx .+. by .*. by
  let s_abc = 2 *. Area.sqrt (a .+. b .+. c)
  let a_2 = Area.sqrt a
  let a_32 = 2 *. a .*. a_2
  let c_2 = 2 *. Area.sqrt c
  let ba = b ./. a_2
  (a_32 .*. s_abc .+. a_2 .*. b .*. (s_abc .-. c_2) .+. (4 *. c .*. a .-. b .*. b) .*. Number.log ((2 *. a_2 .+. ba .+. s_abc) ./. (ba .+. c_2))) ./. (4 *. a_32)

testCubicSplineParameterization :: IO ()
testCubicSplineParameterization = Tolerance.using Length.nanometer do
  let p1 = Point2d.centimeters 5 5
  let p2 = Point2d.centimeters 14 15
  let p3 = Point2d.centimeters 16 15
  let p4 = Point2d.centimeters 25 5
  let spline = Curve2d.cubicBezier p1 p2 p3 p4
  let (parameterized, length) = Curve2d.parameterizeByArcLength spline
  IO.printLine ("Cubic spline: " <> formatLength length)
  let drawCurve fileName curve = do
        let pointLocations = List.map (Curve2d.evaluate curve) (Parameter.steps 30)
        let drawPoint point =
              Drawing2d.circleWith
                [Drawing2d.whiteFill]
                (#centerPoint point)
                (#diameter (Length.millimeters 3))
        let drawingBounds = Bounds2d.hull2 Point2d.origin (Point2d.centimeters 30 15)
        let resolution = Resolution.maxError Length.micrometer
        Drawing2d.writeSvg fileName drawingBounds $
          Drawing2d.group
            [ Drawing2d.curve resolution curve
            , Drawing2d.combine drawPoint pointLocations
            ]
  drawCurve "executables/arc-length/cubic-spline.svg" spline
  drawCurve "executables/arc-length/parameterized-spline.svg" parameterized

main :: IO ()
main = do
  testLineLength
  testQuadraticSplineLength
  testCubicSplineParameterization
