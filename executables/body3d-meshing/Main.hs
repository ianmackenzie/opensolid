module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d
import Prelude hiding (length)

main :: IO ()
main = Tolerance.using Length.nanometer do
  let radius = Length.meters 1
  let length = Length.meters 4
  let arc =
        Curve2d.polarArc
          (#centerPoint Point2d.origin)
          (#radius radius)
          (#startAngle (Angle.degrees -45))
          (#endAngle (Angle.degrees 225))
  let line = Curve2d.line arc.endPoint arc.startPoint
  profile <- Result.orFail (Region2d.boundedBy [arc, line])
  body <- Result.orFail (Body3d.extruded World3d.frontPlane profile (-0.5 *. length) (0.5 *. length))
  let resolution = Resolution.maxSize (Length.centimeters 30)
  let mesh = Body3d.toPointMesh resolution body
  let outputPath = "executables/body3d-meshing/mesh.stl"
  Stl.writeBinary outputPath Convention3d.yUp Length.inMillimeters mesh
