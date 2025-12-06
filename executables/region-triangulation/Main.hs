module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Svg qualified as Svg
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))

main :: IO ()
main = Tolerance.using Length.nanometer do
  let width = Length.centimeters 18
  let height = Length.centimeters 12
  let cornerRadius = Length.centimeters 5
  let holeDiameter = Length.centimeters 8
  let p0 = Point2D.origin
  let p1 = Point2D.x width
  let p2 = Point2D width (height .-. cornerRadius)
  let p3 = Point2D (width .-. cornerRadius) height
  let p4 = Point2D.y height
  let holeCenter = Point2D (width .-. cornerRadius) (height .-. cornerRadius)
  region <- Result.orFail do
    Region2d.boundedBy
      [ Curve2d.line p0 p1
      , Curve2d.line p1 p2
      , Curve2d.arc p2 p3 Angle.quarterTurn
      , Curve2d.line p3 p4
      , Curve2d.line p4 p0
      , Curve2d.circle (#centerPoint holeCenter) (#diameter holeDiameter)
      ]
  let resolution = Resolution.maxError (Length.millimeters 1)
  let mesh = Region2d.toMesh resolution region
  let triangles = Mesh.faceVertices mesh
  let drawingBounds = Bounds2d.hull2 (Point2D.centimeters -3 -3) (Point2D.centimeters 21 15)
  let drawTriangle (a, b, c) =
        Svg.triangleWith
          [ Svg.fillColor Color.lightGrey
          , Svg.strokeColor Color.lightBlue
          , Svg.strokeWidth (Length.millimeters 0.1)
          , Svg.roundStrokeJoins
          ]
          (Triangle2d a b c)
  Svg.write "executables/region-triangulation/triangulated.svg" drawingBounds do
    Svg.combine drawTriangle triangles
