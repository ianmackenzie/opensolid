module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let world = Frame3d.world
  let radius = Length.meters 1.0
  let length = Length.meters 4.0
  let arc =
        Curve2d.polarArc do
          #centerPoint Point2d.origin
          #radius radius
          #startAngle (Angle.degrees -45.0)
          #endAngle (Angle.degrees 225.0)
  let line = Curve2d.line arc.endPoint arc.startPoint
  profile <- Region2d.boundedBy [arc, line]
  let extrusionLimits = Bounds.symmetric (#width length)
  body <- Body3d.extruded world.frontPlane profile extrusionLimits
  let resolution = Resolution.maxSize (Length.centimeters 30.0)
  let mesh = Body3d.toMesh resolution body
  let outputPath = "executables/body3d-meshing/mesh.stl"
  Stl.writeBinary outputPath Convention3d.yUp Length.inMillimeters mesh
