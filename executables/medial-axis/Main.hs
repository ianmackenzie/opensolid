module Main (main) where

import Data.Text (Text)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.MedialAxis qualified as Curve2d.MedialAxis
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Syntax ((*.), type (@))
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance (Tolerance)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)

data Global

main :: IO ()
main = Tolerance.using Length.micrometer do
  timer <- Timer.start
  IO.Parallel.run [testSplineAndArc, testSplineAndLine]
  elapsed <- Timer.elapsed timer
  IO.printLine ("Overall" <> ": " <> Text.int (round (Duration.inMilliseconds elapsed)) <> " ms")

testSplineAndArc :: Tolerance Meters => IO ()
testSplineAndArc = do
  let spline =
        Curve2d.cubicBezier
          (Point2d.centimeters 0 10)
          (Point2d.centimeters 5 6)
          (Point2d.centimeters 10 9)
          (Point2d.centimeters 15 7)
  let arc = Curve2d.arc (Point2d.centimeters 15 0) Point2d.origin (Angle.degrees 20)
  testCurveMedialAxis "testSplineAndArc" spline arc

testSplineAndLine :: Tolerance Meters => IO ()
testSplineAndLine = do
  let spline =
        Curve2d.cubicBezier
          (Point2d.centimeters 15 15)
          (Point2d.centimeters 10 10)
          (Point2d.centimeters 10 10)
          (Point2d.centimeters 5 15)
  let line = Curve2d.line Point2d.origin (Point2d.centimeters 20 0)
  testCurveMedialAxis "testSplineAndLine" spline line

testCurveMedialAxis ::
  Tolerance Meters =>
  Text ->
  Curve2d (Global @ Meters) ->
  Curve2d (Global @ Meters) ->
  IO ()
testCurveMedialAxis label curve1 curve2 = do
  timer <- Timer.start
  segments <- IO.try (Curve2d.medialAxis curve1 curve2)
  let drawTangentCircles (segment :: Curve2d.MedialAxis.Segment (Global @ Meters)) = do
        let (parameterization, _) = Curve2d.arcLengthParameterization segment.curve
        let drawTangentCircle u = do
              let t = Curve.evaluate parameterization u
              let centerPoint = Curve2d.evaluate segment.curve t
              let diameter = 2 *. Quantity.abs (Curve.evaluate segment.radius t)
              Drawing2d.circleWith
                [Drawing2d.strokeColor Color.gray, Drawing2d.strokeWidth (Length.millimeters 0.2)]
                (#centerPoint centerPoint)
                (#diameter diameter)
        Drawing2d.combine drawTangentCircle (Parameter.steps 50)
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let drawCurve = Drawing2d.curve resolution
  let drawSegment segment = drawCurve segment.curve
  let drawingBounds =
        Bounds2d.hull2 (Point2d.centimeters -10 -10) (Point2d.centimeters 30 20)
  Drawing2d.writeSvg ("executables/medial-axis/" <> label <> ".svg") drawingBounds $
    Drawing2d.group
      [ Drawing2d.combine drawTangentCircles segments
      , Drawing2d.combine drawSegment segments
      , drawCurve curve1
      , drawCurve curve2
      ]
  elapsed <- Timer.elapsed timer
  IO.printLine (label <> ": " <> Text.int (round (Duration.inMilliseconds elapsed)) <> " ms")
