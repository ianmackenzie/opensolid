module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.MedialAxis qualified as Curve2d.MedialAxis
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Try qualified as Try

data Global

main :: IO ()
main = Tolerance.using Length.micrometer IO.do
  timer <- Timer.start
  IO.Parallel.run [testSplineAndArc, testSplineAndLine]
  elapsed <- Timer.elapsed timer
  IO.printLine ("Overall" <> ": " <> Text.int (Float.round (Duration.inMilliseconds elapsed)) <> " ms")

testSplineAndArc :: Tolerance Meters => IO ()
testSplineAndArc = do
  let spline =
        Curve2d.cubicBezier
          (Point2d.centimeters @Global 0.0 10.0)
          (Point2d.centimeters @Global 5.0 6.0)
          (Point2d.centimeters @Global 10.0 9.0)
          (Point2d.centimeters @Global 15.0 7.0)
  let arc = Curve2d.arc (Point2d.centimeters 15.0 0.0) Point2d.origin (Angle.degrees 20.0)
  testCurveMedialAxis "testSplineAndArc" spline arc

testSplineAndLine :: Tolerance Meters => IO ()
testSplineAndLine = do
  let spline =
        Curve2d.cubicBezier
          (Point2d.centimeters 15.0 15.0)
          (Point2d.centimeters 10.0 10.0)
          (Point2d.centimeters 10.0 10.0)
          (Point2d.centimeters 5.0 15.0)
  let line = Curve2d.line Point2d.origin (Point2d.centimeters 20.0 0.0)
  testCurveMedialAxis "testSplineAndLine" spline line

testCurveMedialAxis ::
  Tolerance Meters =>
  Text ->
  Curve2d (Global @ Meters) ->
  Curve2d (Global @ Meters) ->
  IO ()
testCurveMedialAxis label curve1 curve2 = IO.do
  timer <- Timer.start
  segments <- Curve2d.medialAxis curve1 curve2
  let drawTangentCircles (segment :: Curve2d.MedialAxis.Segment (Global @ Meters)) = Try.do
        (parameterization, _) <- Curve2d.arcLengthParameterization segment.curve
        let drawTangentCircle u = do
              let t = Curve.evaluate parameterization u
              let centerPoint = Curve2d.evaluate segment.curve t
              let diameter = 2.0 * Qty.abs (Curve.evaluate segment.radius t)
              Drawing2d.circle (#centerPoint centerPoint, #diameter diameter)
        let parameterValues = Parameter.steps 50
        Success (List.map drawTangentCircle parameterValues)
  tangentCircleLists <- Result.collect drawTangentCircles segments
  let allTangentCircles = List.concat tangentCircleLists
  let tangentCircleAttributes =
        [ Drawing2d.strokeColor Color.gray
        , Drawing2d.strokeWidth (Length.millimeters 0.2)
        ]
  let drawCurve = Drawing2d.curve (Length.millimeters 0.1)
  let drawSegment segment = drawCurve segment.curve
  let drawingBounds =
        Bounds2d.hull2 (Point2d.centimeters -10.0 -10.0) (Point2d.centimeters 30.0 20.0)
  Drawing2d.writeSvg ("executables/medial-axis/" <> label <> ".svg") drawingBounds do
    Drawing2d.groupWith tangentCircleAttributes allTangentCircles
    Drawing2d.collect drawSegment segments
    drawCurve curve1
    drawCurve curve2
  elapsed <- Timer.elapsed timer
  IO.printLine (label <> ": " <> Text.int (Float.round (Duration.inMilliseconds elapsed)) <> " ms")
