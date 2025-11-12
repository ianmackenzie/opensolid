module Main (main) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

main :: IO ()
main = Tolerance.using Length.nanometer do
  let weightCurve = Curve.quadraticSpline 1 (1 /. Number.sqrt 2) 1
  let vE = Vector2d 1 0
  let vNE = Vector2d 1 1 ./. Number.sqrt 2
  let vN = Vector2d 0 1
  let vNW = Vector2d -1 1 ./. Number.sqrt 2
  let vW = Vector2d -1 0
  let vSW = Vector2d -1 -1 ./. Number.sqrt 2
  let vS = Vector2d 0 -1
  let vSE = Vector2d 1 -1 ./. Number.sqrt 2
  let radius = Length.centimeters 10
  let arc v1 v2 v3 = do
        radialUnitVector <- Result.orFail do
          Tolerance.using 1e-9 $
            VectorCurve2d.quotient
              (VectorCurve2d.quadraticBezier v1 v2 v3)
              weightCurve
        return (Point2d.origin .+. radius .*. radialUnitVector)
  arc1 <- arc vE vNE vN
  arc2 <- arc vN vNW vW
  arc3 <- arc vW vSW vS
  arc4 <- arc vS vSE vE
  let circle = Curve2d.piecewise (NonEmpty.four arc1 arc2 arc3 arc4)
  let drawDot point =
        Drawing2d.circleWith
          [Drawing2d.whiteFill]
          (#centerPoint point)
          (#diameter (Length.millimeters 4))
  let drawCurve n curve =
        Drawing2d.group
          [ Drawing2d.curve (Resolution.maxError Length.micrometer) curve
          , Drawing2d.combine (drawDot . Curve2d.evaluate curve) (Parameter.steps n)
          ]
  let drawingBounds =
        Bounds2d.hull2 (Point2d.centimeters -12 -12) (Point2d.centimeters 12 12)
  Drawing2d.writeSvg "executables/piecewise-curve/circle.svg" drawingBounds (drawCurve 40 circle)
  Drawing2d.writeSvg "executables/piecewise-curve/arcs.svg" drawingBounds $
    Drawing2d.combine (drawCurve 10) [arc1, arc2, arc3, arc4]
