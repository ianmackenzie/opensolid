module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer do
  let width = Length.centimeters 18
  let height = Length.centimeters 12
  let cornerRadius = Length.centimeters 5
  let holeDiameter = Length.centimeters 8
  let p0 = Point2d.origin
  let p1 = Point2d.x width
  let p2 = Point2d width (height .-. cornerRadius)
  let p3 = Point2d (width .-. cornerRadius) height
  let p4 = Point2d.y height
  let holeCenter = Point2d (width .-. cornerRadius) (height .-. cornerRadius)
  region <- IO.try do
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
  let drawingBounds = Bounds2d.hull2 (Point2d.centimeters -3 -3) (Point2d.centimeters 21 15)
  let drawTriangle (a, b, c) =
        Drawing2d.polygonWith
          [ Drawing2d.fillColor Color.lightGrey
          , Drawing2d.strokeColor Color.lightBlue
          , Drawing2d.strokeWidth (Length.millimeters 0.1)
          , Drawing2d.roundStrokeJoins
          ]
          [a, b, c]
  Drawing2d.writeSvg "executables/region-triangulation/triangulated.svg" drawingBounds do
    Drawing2d.combine drawTriangle triangles
