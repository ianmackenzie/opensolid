module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  let p1 = Point2d.origin
  let p2 = Point2d.centimeters 20.0 0.0
  let p3 = Point2d.centimeters 20.0 10.0
  let p4 = Point2d.centimeters 0.0 10.0
  let spline = Curve2d.cubicBezier p1 p2 p3 p4
  profile <- Region2d.boundedBy [spline, Curve2d.line p4 p1]
  body <- Body3d.revolved World3d.rightPlane profile Axis2d.y Angle.twoPi
  let model = Model3d.body body
  let resolution = Resolution.maxError (Length.millimeters 0.05)
  Gltf.writeBinary "executables/oblate-spheroid/mesh.glb" model resolution
