module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Point2D (data Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Stl qualified as Stl
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = spatial do
  let innerRadius = Length.centimeters 10.0
  let width = Length.centimeters 3.0
  let outerRadius = innerRadius + width
  let thickness = Length.millimeters 10.0
  let p0 = Point2D.x innerRadius
  let p1 = Point2D.x outerRadius
  let p2 = Point2D outerRadius thickness
  let p3 = Point2D (innerRadius + thickness) width
  let p4 = Point2D innerRadius width
  line01 <- try do Curve2D.lineFrom p0 p1
  line12 <- try do Curve2D.lineFrom p1 p2
  arc23 <- try do Curve2D.arcFrom p2 p3 -Angle.quarterTurn
  line34 <- try do Curve2D.lineFrom p3 p4
  line40 <- try do Curve2D.lineFrom p4 p0
  profile <- try do Region2D.boundedBy [line01, line12, arc23, line34, line40]
  body <- try do Body3D.revolved World3D.rightPlane profile Axis2D.y (Angle.degrees 270.0)
  let resolution = Resolution.maxError (Length.millimeters 0.2)
  let mesh = Body3D.toPointMesh resolution body
  Stl.writeBinary "executables/funky-moulding/mesh.stl" Convention3D.yUp Length.inMillimeters mesh
