module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2D (data Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Svg qualified as Svg
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))

main :: IO ()
main = Tolerance.using Length.defaultTolerance do
  let width = Length.centimeters 18.0
  let height = Length.centimeters 12.0
  let cornerRadius = Length.centimeters 5.0
  let holeDiameter = Length.centimeters 8.0
  let p0 = Point2D.origin
  let p1 = Point2D.x width
  let p2 = Point2D width (height - cornerRadius)
  let p3 = Point2D (width - cornerRadius) height
  let p4 = Point2D.y height
  let holeCenter = Point2D (width - cornerRadius) (height - cornerRadius)
  region <-
    Region2D.boundedBy
      [ Curve2D.lineFrom p0 p1
      , Curve2D.lineFrom p1 p2
      , Curve2D.arcFrom p2 p3 Angle.quarterTurn
      , Curve2D.lineFrom p3 p4
      , Curve2D.lineFrom p4 p0
      , Curve2D.circle (Circle2D.withDiameter holeDiameter holeCenter)
      ]
      & Result.orFail
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  Svg.write "executables/region-triangulation/region.svg" (Svg.padding Length.centimeter) do
    Svg.regionWith [Svg.blackStroke, Svg.fillColor Color.lightGrey] resolution region
  let mesh = Region2D.toMesh resolution region
  let triangles = Mesh.faceVertices mesh
  let drawTriangle (a, b, c) =
        Svg.triangleWith
          [ Svg.fillColor Color.lightGrey
          , Svg.strokeColor Color.lightBlue
          , Svg.strokeWidth (Length.millimeters 0.1)
          , Svg.roundStrokeJoins
          ]
          (Triangle2D a b c)
  Svg.write "executables/region-triangulation/triangulated.svg" (Svg.padding Length.centimeter) do
    Svg.combine drawTriangle triangles
