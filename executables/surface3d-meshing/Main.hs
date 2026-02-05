module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = do
  let r = Length.meters 1.0
  let h = Number.twoPi * r
  let theta = Angle.twoPi * SurfaceFunction1D.u
  let surfaceFunction =
        World3D.originPoint
          + r * SurfaceFunction1D.cos theta * World3D.rightwardDirection
          + r * SurfaceFunction1D.sin theta * World3D.forwardDirection
          + h * SurfaceFunction1D.v * World3D.upwardDirection
  let domainCenter = UvPoint 0.5 0.5
  let domainDiameter = 2 / 3
  let domainCircle = Curve2D.circle (Circle2D.withDiameter domainDiameter domainCenter)
  domain <- Result.orFail (Tolerance.using 1e-9 (Region2D.boundedBy [domainCircle]))
  let surface = Surface3D.parametric surfaceFunction domain
  let mesh = Surface3D.toMesh (Length.millimeters 2.0) surface
  IO.printLine ("Num faces: " <> Text.int (Mesh.numFaces mesh))
  let path = "executables/surface3D-meshing/mesh.stl"
  Stl.writeBinary path Convention3D.yUp Length.inMillimeters mesh
