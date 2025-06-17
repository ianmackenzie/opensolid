module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = IO.do
  let world = Frame3d.world
  let r = Length.meters 1.0
  let h = Float.twoPi * r
  let theta = Angle.twoPi * SurfaceFunction.u
  let surfaceFunction =
        world.originPoint
          + r * SurfaceFunction.cos theta * world.rightwardDirection
          + r * SurfaceFunction.sin theta * world.forwardDirection
          + h * SurfaceFunction.v * world.upwardDirection
  let domainCenter = Point2d 0.5 0.5
  let domainDiameter = 2 / 3
  let domainCircle = Curve2d.circle (#centerPoint domainCenter, #diameter domainDiameter)
  domain <- Tolerance.using 1e-9 (Region2d.boundedBy [domainCircle])
  let surface = Surface3d.parametric surfaceFunction domain
  let mesh = Surface3d.toMesh (Length.millimeters 2.0) surface
  IO.printLine ("Num faces: " <> Text.int mesh.numFaces)
  let path = "executables/surface3d-meshing/mesh.stl"
  Stl.writeBinary path Convention3d.yUp Length.inMillimeters mesh
