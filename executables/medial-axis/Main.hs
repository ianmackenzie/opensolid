module Main (main) where

import Data.Time.Clock qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2d qualified as Arc2d
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.CubicSpline2d qualified as CubicSpline2d
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.MedialAxis qualified as Curve2d.MedialAxis
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)

data Global

main :: IO ()
main = Tolerance.using Length.micrometer IO.do
  testSplineAndArc
  testSplineAndLine

testSplineAndArc :: Tolerance Meters => IO ()
testSplineAndArc = do
  let spline =
        CubicSpline2d.fromControlPoints
          (Point2d.centimeters @Global 0.0 10.0)
          (Point2d.centimeters @Global 5.0 6.0)
          (Point2d.centimeters @Global 10.0 9.0)
          (Point2d.centimeters @Global 15.0 7.0)
  let arc = Arc2d.from (Point2d.centimeters 15.0 0.0) Point2d.origin (Angle.degrees 20.0)
  testCurveMedialAxis "testSplineAndArc" spline arc

testSplineAndLine :: Tolerance Meters => IO ()
testSplineAndLine = do
  let spline =
        CubicSpline2d.fromControlPoints
          (Point2d.centimeters 15.0 15.0)
          (Point2d.centimeters 10.0 10.0)
          (Point2d.centimeters 10.0 10.0)
          (Point2d.centimeters 5.0 15.0)
  let line = Line2d.from Point2d.origin (Point2d.centimeters 20.0 0.0)
  testCurveMedialAxis "testSplineAndLine" spline line

testCurveMedialAxis :: Tolerance Meters => Text -> Curve2d (Global @ Meters) -> Curve2d (Global @ Meters) -> IO ()
testCurveMedialAxis label curve1 curve2 = IO.do
  startTime <- Data.Time.Clock.getCurrentTime
  segments <- Curve2d.medialAxis curve1 curve2
  let drawCircles (segment :: Curve2d.MedialAxis.Segment (Global @ Meters)) = Result.do
        let curve = Curve2d.MedialAxis.curve segment
        (parameterization, _) <- Curve2d.arcLengthParameterization curve
        let drawTangentCircle u = do
              let t = Curve1d.evaluate parameterization u
              let tangentCircle =
                    Drawing2d.circle
                      [ Drawing2d.strokeColor Color.gray
                      , Drawing2d.strokeWidth (Length.millimeters 0.2)
                      ]
                      (Qty.abs (Curve1d.evaluate (Curve2d.MedialAxis.radius segment) t))
                      (Curve2d.evaluate curve t)
              tangentCircle
        Success (List.map drawTangentCircle (Parameter.steps 50))
  tangentCircles <- Result.collect drawCircles segments
  let drawCurve = Drawing2d.curve [] (Length.millimeters 0.1)
  let drawingBounds =
        Bounds2d.hull2 (Point2d.centimeters -10.0 -10.0) (Point2d.centimeters 30.0 20.0)
  Drawing2d.writeTo ("executables/sandbox/" + label + ".svg") drawingBounds $
    [ Drawing2d.group (List.concat tangentCircles)
    , Drawing2d.group (List.map (drawCurve . Curve2d.MedialAxis.curve) segments)
    , drawCurve curve1
    , drawCurve curve2
    ]
  endTime <- Data.Time.Clock.getCurrentTime
  let elapsed = Duration.from startTime endTime
  IO.printLine (label + ": " + Text.int (Float.round (Duration.inMilliseconds elapsed)) + " ms")