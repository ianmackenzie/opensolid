module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D

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
    Region2D.boundedBy
      [ Curve2D.lineFrom p0 p1
      , Curve2D.lineFrom p1 p2
      , Curve2D.arcFrom p2 p3 (negative Angle.quarterTurn)
      , Curve2D.lineFrom p3 p4
      , Curve2D.lineFrom p4 p0
      ]
  body <- Result.orFail (Body3D.revolved World3D.rightPlane profile Axis2D.y (Angle.degrees 270))
  let resolution = Resolution.maxError (Length.millimeters 0.2)
  let mesh = Body3D.toPointMesh resolution body
  Stl.writeBinary "executables/funky-moulding/mesh.stl" Convention3D.yUp Length.inMillimeters mesh
