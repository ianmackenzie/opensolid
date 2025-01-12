module Main (main) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let weightCurve = Curve.quadraticSpline 1.0 (1.0 / Float.sqrt 2.0) 1.0
  let vE = Vector2d.xy 1.0 0.0
  let vNE = Vector2d.xy 1.0 1.0 / Float.sqrt 2.0
  let vN = Vector2d.xy 0.0 1.0
  let vNW = Vector2d.xy -1.0 1.0 / Float.sqrt 2.0
  let vW = Vector2d.xy -1.0 0.0
  let vSW = Vector2d.xy -1.0 -1.0 / Float.sqrt 2.0
  let vS = Vector2d.xy 0.0 -1.0
  let vSE = Vector2d.xy 1.0 -1.0 / Float.sqrt 2.0
  let radius = Length.centimeters 10.0
  let arc1 = Point2d.origin + radius * VectorCurve2d.quadraticSpline vE vNE vN / weightCurve
  let arc2 = Point2d.origin + radius * VectorCurve2d.quadraticSpline vN vNW vW / weightCurve
  let arc3 = Point2d.origin + radius * VectorCurve2d.quadraticSpline vW vSW vS / weightCurve
  let arc4 = Point2d.origin + radius * VectorCurve2d.quadraticSpline vS vSE vE / weightCurve
  circle <- Curve2d.piecewise (NonEmpty.four arc1 arc2 arc3 arc4)
  let drawCurve n curve =
        Drawing2d.group
          [ Drawing2d.curve [] Length.micrometer curve
          , Drawing2d.group $
              List.map (Drawing2d.circle [Drawing2d.whiteFill] (Length.millimeters 2.0)) $
                List.map (Curve2d.evaluate curve) (Parameter.steps n)
          ]
  let drawingBounds = Bounds2d.hull2 (Point2d.centimeters -12.0 -12.0) (Point2d.centimeters 12.0 12.0)
  Drawing2d.writeTo "executables/piecewise-curve/arcs.svg" drawingBounds $
    List.map (drawCurve 10) [arc1, arc2, arc3, arc4]
  Drawing2d.writeTo "executables/piecewise-curve/circle.svg" drawingBounds [drawCurve 40 circle]
