module Main (main) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let weightCurve = Curve.quadraticSpline 1.0 (1.0 / Float.sqrt 2.0) 1.0
  let vE = Vector2d 1.0 0.0
  let vNE = Vector2d 1.0 1.0 / Float.sqrt 2.0
  let vN = Vector2d 0.0 1.0
  let vNW = Vector2d -1.0 1.0 / Float.sqrt 2.0
  let vW = Vector2d -1.0 0.0
  let vSW = Vector2d -1.0 -1.0 / Float.sqrt 2.0
  let vS = Vector2d 0.0 -1.0
  let vSE = Vector2d 1.0 -1.0 / Float.sqrt 2.0
  let radius = Length.centimeters 10.0
  let arc v1 v2 v3 = Result.do
        radialUnitVector <-
          Tolerance.using 1e-9 $
            VectorCurve2d.quotient
              @ VectorCurve2d.quadraticBezier v1 v2 v3
              @ weightCurve
        Success (Point2d.origin + radius * radialUnitVector)
  arc1 <- arc vE vNE vN
  arc2 <- arc vN vNW vW
  arc3 <- arc vW vSW vS
  arc4 <- arc vS vSE vE
  let circle = Curve2d.piecewise (NonEmpty.four arc1 arc2 arc3 arc4)
  let drawDot point =
        Drawing2d.circleWith [Drawing2d.whiteFill] do
          #centerPoint point
          #diameter (Length.millimeters 4.0)
  let drawCurve n curve = do
        Drawing2d.curve (Resolution.maxError Length.micrometer) curve
        Drawing2d.collect (drawDot . Curve2d.evaluate curve) (Parameter.steps n)
  let drawingBounds = Bounds2d.hull2 (Point2d.centimeters -12.0 -12.0) (Point2d.centimeters 12.0 12.0)
  Drawing2d.writeSvg "executables/piecewise-curve/circle.svg" drawingBounds (drawCurve 40 circle)
  Drawing2d.writeSvg "executables/piecewise-curve/arcs.svg" drawingBounds do
    Drawing2d.collect (drawCurve 10) [arc1, arc2, arc3, arc4]
