module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.MedialAxis qualified as Curve2d.MedialAxis
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
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
          (Point2D.centimeters 0 10)
          (Point2D.centimeters 5 6)
          (Point2D.centimeters 10 9)
          (Point2D.centimeters 15 7)
  let arc = Curve2d.arcFrom (Point2D.centimeters 15 0) Point2D.origin (Angle.degrees 20)
  testCurveMedialAxis "testSplineAndArc" spline arc

testSplineAndLine :: Tolerance Meters => IO ()
testSplineAndLine = do
  let spline =
        Curve2d.cubicBezier
          (Point2D.centimeters 15 15)
          (Point2D.centimeters 10 10)
          (Point2D.centimeters 10 10)
          (Point2D.centimeters 5 15)
  let line = Curve2d.lineFrom Point2D.origin (Point2D.centimeters 20 0)
  testCurveMedialAxis "testSplineAndLine" spline line

testCurveMedialAxis ::
  Tolerance Meters =>
  Text ->
  Curve2d Meters Global ->
  Curve2d Meters Global ->
  IO ()
testCurveMedialAxis label curve1 curve2 = do
  timer <- Timer.start
  segments <- Result.orFail (Curve2d.medialAxis curve1 curve2)
  let drawTangentCircles (segment :: Curve2d.MedialAxis.Segment Meters Global) = do
        let (parameterization, _) = Curve2d.arcLengthParameterization segment.curve
        let drawTangentCircle u = do
              let t = Curve.evaluate parameterization u
              let centerPoint = Curve2d.evaluate segment.curve t
              let diameter = 2 *. Quantity.abs (Curve.evaluate segment.radius t)
              Svg.circleWith
                [Svg.strokeColor Color.gray, Svg.strokeWidth (Length.millimeters 0.2)]
                (#centerPoint centerPoint)
                (#diameter diameter)
        Svg.combine drawTangentCircle (Parameter.steps 50)
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let drawCurve = Svg.curve resolution
  let drawSegment segment = drawCurve segment.curve
  let drawingBounds =
        Bounds2d.hull2 (Point2D.centimeters -10 -10) (Point2D.centimeters 30 20)
  Svg.write ("executables/medial-axis/" <> label <> ".svg") drawingBounds $
    Svg.group
      [ Svg.combine drawTangentCircles segments
      , Svg.combine drawSegment segments
      , drawCurve curve1
      , drawCurve curve2
      ]
  elapsed <- Timer.elapsed timer
  IO.printLine (label <> ": " <> Text.int (round (Duration.inMilliseconds elapsed)) <> " ms")
