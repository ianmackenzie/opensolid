module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.IO qualified as IO
import OpenSolid.Labels
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  let radius = Length.meters 1.0
  let length = Length.meters 4.0
  let arc =
        Curve2d.polarArc
          & CenterPoint Point2d.origin
          & Radius radius
          & StartAngle (Angle.degrees -45.0)
          & EndAngle (Angle.degrees 225.0)
  let line = Curve2d.line (Curve2d.endPoint arc) (Curve2d.startPoint arc)
  profile <- Region2d.boundedBy [arc, line]
  let extrusionLimits = Range (-0.5 * length) (0.5 * length)
  body <- Body3d.extruded Plane3d.yz profile extrusionLimits
  let constraints = NonEmpty.one (Mesh.maxSize (Length.centimeters 30.0))
  let mesh = Body3d.toMesh constraints body
  Stl.writeBinary "executables/body3d-meshing/mesh.stl" Length.inMillimeters mesh
