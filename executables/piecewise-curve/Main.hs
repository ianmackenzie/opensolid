module Main (main) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Svg qualified as Svg
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

main :: IO ()
main = Tolerance.using Length.nanometer do
  let weightCurve = Curve1D.quadraticSpline 1.0 (1.0 / Number.sqrt 2.0) 1.0
  let vE = Vector2D 1.0 0.0
  let vNE = Vector2D 1.0 1.0 / Number.sqrt 2.0
  let vN = Vector2D 0.0 1.0
  let vNW = Vector2D -1.0 1.0 / Number.sqrt 2.0
  let vW = Vector2D -1.0 0.0
  let vSW = Vector2D -1.0 -1.0 / Number.sqrt 2.0
  let vS = Vector2D 0.0 -1.0
  let vSE = Vector2D 1.0 -1.0 / Number.sqrt 2.0
  let radius = Length.centimeters 10.0
  let arc v1 v2 v3 = do
        radialUnitVector <- Result.orFail do
          Tolerance.using 1e-9 $
            VectorCurve2D.quotient
              (VectorCurve2D.quadraticBezier v1 v2 v3)
              weightCurve
        IO.succeed (Point2D.origin + radius * radialUnitVector)
  arc1 <- arc vE vNE vN
  arc2 <- arc vN vNW vW
  arc3 <- arc vW vSW vS
  arc4 <- arc vS vSE vE
  let circle = Curve2D.piecewise (NonEmpty.four arc1 arc2 arc3 arc4)
  let drawDot point = do
        let diameter = Length.millimeters 4.0
        Svg.circleWith [Svg.whiteFill] (#centerPoint point) (#diameter diameter)
  let drawCurve n curve =
        Svg.group
          [ Svg.curve (Resolution.maxError Length.micrometer) curve
          , Svg.combine (drawDot . Curve2D.evaluate curve) (Parameter.steps n)
          ]
  let drawingBounds =
        Bounds2D.hull2 (Point2D.centimeters -12.0 -12.0) (Point2D.centimeters 12.0 12.0)
  Svg.write "executables/piecewise-curve/circle.svg" drawingBounds (drawCurve 40 circle)
  Svg.write "executables/piecewise-curve/arcs.svg" drawingBounds $
    Svg.combine (drawCurve 10) [arc1, arc2, arc3, arc4]
