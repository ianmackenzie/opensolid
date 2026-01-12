module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length qualified as Length
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using Length.nanometer do
  let p1 = Point2D.origin
  let p2 = Point2D.centimeters 20 0
  let p3 = Point2D.centimeters 20 10
  let p4 = Point2D.centimeters 0 10
  let spline = Curve2D.cubicBezier p1 p2 p3 p4
  profile <- Result.orFail (Region2D.boundedBy [spline, Curve2D.lineFrom p4 p1])
  body <- Result.orFail (Body3D.revolved World3D.rightPlane profile Axis2D.y Angle.twoPi)
  let model = Model3D.body body
  let resolution = Resolution.maxError (Length.millimeters 0.05)
  Gltf.writeBinary "executables/oblate-spheroid/mesh.glb" model resolution
