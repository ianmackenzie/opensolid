module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let width = Length.centimeters 18.0
  let height = Length.centimeters 12.0
  let cornerRadius = Length.centimeters 5.0
  let holeRadius = Length.centimeters 4.0
  let p0 = Point2d.origin
  let p1 = Point2d.x width
  let p2 = Point2d.xy width (height - cornerRadius)
  let p3 = Point2d.xy (width - cornerRadius) height
  let p4 = Point2d.y height
  let holeCenter = Point2d.xy (width - cornerRadius) (height - cornerRadius)
  region <-
    Region2d.boundedBy
      [ Curve2d.line p0 p1
      , Curve2d.line p1 p2
      , Curve2d.arc p2 p3 Angle.quarterTurn
      , Curve2d.line p3 p4
      , Curve2d.line p4 p0
      , Curve2d.circle holeCenter holeRadius
      ]
  let polygon = Region2d.toPolygon (Length.millimeters 1.0) Curve2d.evaluate region
  let outerLoop = Polygon2d.outerLoop polygon
  let innerLoops = Polygon2d.innerLoops polygon
  let steinerPoints =
        [ NonEmpty.one (Point2d.centimeters 3.0 3.0)
        , NonEmpty.one (Point2d.centimeters 3.0 9.0)
        ]
  let modifiedPolygon = Polygon2d.withHoles (innerLoops + steinerPoints) outerLoop
  let mesh = Polygon2d.triangulate modifiedPolygon
  let triangles = Mesh.faceVertices mesh
  let drawingBounds = Bounds2d.hull2 (Point2d.centimeters -3.0 -3.0) (Point2d.centimeters 21.0 15.0)
  let drawTriangle (a, b, c) =
        Drawing2d.polygon
          [ Drawing2d.fillColor Color.lightGrey
          , Drawing2d.strokeColor Color.lightBlue
          , Drawing2d.strokeWidth (Length.millimeters 0.1)
          , Drawing2d.roundStrokeJoins
          ]
          [a, b, c]
  Drawing2d.writeTo "executables/region-triangulation/triangulated.svg" drawingBounds $
    List.map drawTriangle triangles
