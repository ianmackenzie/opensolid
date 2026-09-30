module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2D (data Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Svg qualified as Svg
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))
import OpenSolid.Try qualified as Try

main :: IO ()
main = spatial Try.do
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
  line01 <- Curve2D.lineFrom p0 p1
  line12 <- Curve2D.lineFrom p1 p2
  arc23 <- Curve2D.arcFrom p2 p3 Angle.quarterTurn
  line34 <- Curve2D.lineFrom p3 p4
  line40 <- Curve2D.lineFrom p4 p0
  hole <- Curve2D.circleWith {centerPoint = holeCenter, diameter = holeDiameter}
  region <- Region2D.boundedBy [line01, line12, arc23, line34, line40, hole]
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  Svg.write "executables/region-triangulation/region.svg" (Svg.padding Length.centimeter) do
    Svg.with [Svg.blackStroke, Svg.fillColor Color.lightGrey] do
      Svg.region resolution region
  let mesh = Region2D.toMesh resolution region
  let triangles = Mesh.faceVertices mesh
  let drawTriangle (a, b, c) =
        Svg.with
          [ Svg.fillColor Color.lightGrey
          , Svg.strokeColor Color.lightBlue
          , Svg.strokeWidth (Length.millimeters 0.1)
          , Svg.roundStrokeJoins
          ]
          do Svg.triangle (Triangle2D a b c)
  Svg.write "executables/region-triangulation/triangulated.svg" (Svg.padding Length.centimeter) do
    Svg.combine drawTriangle triangles
