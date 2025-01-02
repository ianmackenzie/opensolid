module Main (main) where

import OpenSolid.Area (Area)
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.QuadraticSpline2d qualified as QuadraticSpline2d
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.Volume (Volume)

data Space

formatLength :: Length -> Text
formatLength length =
  Text.float (Length.inMeters length) + "m"

testCurve :: Text -> Curve2d (Space @ Meters) -> IO ()
testCurve label curve = Tolerance.using (Length.meters 1e-12) IO.do
  length <- Curve2d.arcLength curve
  IO.printLine (label + ": " + formatLength length)

testLine :: IO ()
testLine = Tolerance.using (Length.meters 1e-6) IO.do
  testCurve "Line" (Line2d.from Point2d.origin (Point2d.centimeters 30.0 40.0))

testQuadraticSpline :: IO ()
testQuadraticSpline = IO.do
  let p1 = Point2d.origin
  let p2 = Point2d.centimeters 20.0 30.0
  let p3 = Point2d.centimeters 40.0 0.0
  let spline = QuadraticSpline2d.fromControlPoints p1 p2 p3
  testCurve "Quadratic spline" spline
  IO.printLine ("Analytical value: " + formatLength (analyticalLength p1 p2 p3))

analyticalLength ::
  Point2d (space @ Meters) ->
  Point2d (space @ Meters) ->
  Point2d (space @ Meters) ->
  Length
analyticalLength p0 p1 p2 = do
  let (x0, y0) = Point2d.coordinates p0
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  let ax :: Length = x0 - 2.0 * x1 + x2
  let ay :: Length = y0 - 2.0 * y1 + y2
  let bx :: Length = 2.0 * x1 - 2.0 * x0
  let by :: Length = 2.0 * y1 - 2.0 * y0
  let a :: Area = 4.0 * (ax * ax + ay * ay)
  let b :: Area = 4.0 * (ax * bx + ay * by)
  let c :: Area = bx * bx + by * by
  let s_abc :: Length = 2.0 * Qty.sqrt (a + b + c)
  let a_2 :: Length = Qty.sqrt a
  let a_32 :: Volume = 2.0 * a * a_2
  let c_2 :: Length = 2.0 * Qty.sqrt c
  let ba :: Length = b / a_2
  ((a_32 * s_abc + a_2 * b * (s_abc - c_2) + (4.0 * c * a - b * b) * Float.log ((2.0 * a_2 + ba + s_abc) / (ba + c_2))) / (4.0 * a_32))

main :: IO ()
main = IO.do
  testLine
  testQuadraticSpline
