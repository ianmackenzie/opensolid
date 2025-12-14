module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Length qualified as Length
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d

main :: IO ()
main = Tolerance.using Length.nanometer do
  let innerRadius = Length.centimeters 10
  let width = Length.centimeters 3
  let outerRadius = innerRadius .+. width
  let thickness = Length.millimeters 10
  let p0 = Point2D.x innerRadius
  let p1 = Point2D.x outerRadius
  let p2 = Point2D outerRadius thickness
  let p3 = Point2D (innerRadius .+. thickness) width
  let p4 = Point2D innerRadius width
  profile <- Result.orFail do
    Region2d.boundedBy
      [ Curve2d.lineFrom p0 p1
      , Curve2d.lineFrom p1 p2
      , Curve2d.arcFrom p2 p3 (negative Angle.quarterTurn)
      , Curve2d.lineFrom p3 p4
      , Curve2d.lineFrom p4 p0
      ]
  body <- Result.orFail (Body3d.revolved World3d.rightPlane profile Axis2d.y (Angle.degrees 270))
  let resolution = Resolution.maxError (Length.millimeters 0.2)
  let mesh = Body3d.toPointMesh resolution body
  Stl.writeBinary "executables/funky-moulding/mesh.stl" Convention3d.yUp Length.inMillimeters mesh
