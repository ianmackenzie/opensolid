module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Color qualified as Color
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.MedialAxis qualified as Curve2D.MedialAxis
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Svg qualified as Svg
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance

data Global

formatDuration :: Duration -> Text
formatDuration duration = Text.int (Number.round (Duration.inMilliseconds duration)) <> " ms"

main :: IO ()
main = Tolerance.using Length.micrometer do
  timer <- Timer.start
  IO.Parallel.run [testSplineAndArc, testSplineAndLine]
  elapsed <- Timer.elapsed timer
  IO.printLine ("Overall" <> ": " <> formatDuration elapsed)

testSplineAndArc :: Tolerance Meters => IO ()
testSplineAndArc = do
  let spline =
        Curve2D.cubicBezier
          (Point2D.centimeters 0.0 10.0)
          (Point2D.centimeters 5.0 6.0)
          (Point2D.centimeters 10.0 9.0)
          (Point2D.centimeters 15.0 7.0)
  let arc = Curve2D.arcFrom (Point2D.centimeters 15.0 0.0) Point2D.origin (Angle.degrees 20.0)
  testCurveMedialAxis "testSplineAndArc" spline arc

testSplineAndLine :: Tolerance Meters => IO ()
testSplineAndLine = do
  let spline =
        Curve2D.cubicBezier
          (Point2D.centimeters 15.0 15.0)
          (Point2D.centimeters 10.0 10.0)
          (Point2D.centimeters 10.0 10.0)
          (Point2D.centimeters 5.0 15.0)
  let line = Curve2D.lineFrom Point2D.origin (Point2D.centimeters 20.0 0.0)
  testCurveMedialAxis "testSplineAndLine" spline line

testCurveMedialAxis ::
  Tolerance Meters =>
  Text ->
  Curve2D Meters Global ->
  Curve2D Meters Global ->
  IO ()
testCurveMedialAxis label curve1 curve2 = do
  timer <- Timer.start
  segments <- Result.orFail (Curve2D.medialAxis curve1 curve2)
  let drawTangentCircles (segment :: Curve2D.MedialAxis.Segment Meters Global) = do
        let (parameterization, _) = Curve2D.arcLengthParameterization segment.curve
        let drawTangentCircle u = do
              let t = Curve1D.evaluate parameterization u
              let centerPoint = Curve2D.evaluate segment.curve t
              let diameter = 2.0 * Quantity.abs (Curve1D.evaluate segment.radius t)
              Svg.circleWith
                [Svg.strokeColor Color.gray, Svg.strokeWidth (Length.millimeters 0.2)]
                (#centerPoint centerPoint)
                (#diameter diameter)
        Svg.combine drawTangentCircle (Parameter.steps 50)
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let drawCurve = Svg.curve resolution
  let drawSegment segment = drawCurve segment.curve
  let drawingBounds =
        Bounds2D.hull2 (Point2D.centimeters -10.0 -10.0) (Point2D.centimeters 30.0 20.0)
  Svg.write ("executables/medial-axis/" <> label <> ".svg") drawingBounds $
    Svg.group
      [ Svg.combine drawTangentCircles segments
      , Svg.combine drawSegment segments
      , drawCurve curve1
      , drawCurve curve2
      ]
  elapsed <- Timer.elapsed timer
  IO.printLine (label <> ": " <> formatDuration elapsed)
