module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Svg qualified as Svg
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D qualified as Vector2D

main :: IO ()
main = Tolerance.using Length.defaultTolerance do
  let baseRadius = Length.centimeters 10.0
  let involuteCurve =
        Curve2D.involute
          Point2D.origin
          (Vector2D.y baseRadius)
          Angle.zero
          (Angle.degrees -45.0)
  let arcRadius = Length.centimeters 1.0
  let arcEnd = Curve2D.startPoint involuteCurve
  let arcStart = arcEnd & Point2D.translateBy (Vector2D.x (-2.0 * arcRadius))
  let arc = Curve2D.arcFrom arcStart arcEnd Angle.pi
  let overallBounds = Bounds2D.aggregate2 (Curve2D.bounds arc) (Curve2D.bounds involuteCurve)
  let resolution = Resolution.maxError (Length.millimeters 0.05)
  Svg.write "executables/degenerate-join/curve.svg" overallBounds $
    Svg.group
      [ Svg.curve resolution involuteCurve
      , Svg.curve resolution arc
      ]
  intersections <- Curve2D.intersections involuteCurve arc & Result.orFail
  IO.printLine (Text.show intersections)
