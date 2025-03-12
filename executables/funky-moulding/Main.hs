module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  let innerRadius = Length.centimeters 10.0
  let width = Length.centimeters 3.0
  let outerRadius = innerRadius + width
  let thickness = Length.millimeters 10.0
  let p0 = Point2d.x innerRadius
  let p1 = Point2d.x outerRadius
  let p2 = Point2d.xy outerRadius thickness
  let p3 = Point2d.xy (innerRadius + thickness) width
  let p4 = Point2d.xy innerRadius width
  profile <-
    Region2d.boundedBy
      [ Curve2d.line p0 p1
      , Curve2d.line p1 p2
      , Curve2d.arc p2 p3 -Angle.quarterTurn
      , Curve2d.line p3 p4
      , Curve2d.line p4 p0
      ]
  body <- Body3d.revolved Plane3d.yz profile Axis2d.y (Angle.degrees 270.0)
  let constraints = NonEmpty.one (Mesh.maxError (Length.millimeters 0.2))
  let mesh = Body3d.toMesh constraints body
  IO.writeUtf8 "executables/funky-moulding/mesh.stl" (Stl.text Length.inMillimeters mesh)
