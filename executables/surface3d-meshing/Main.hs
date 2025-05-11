module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = IO.do
  let r = Length.meters 1.0
  let h = Float.twoPi * r
  let theta = Angle.twoPi * SurfaceFunction.u
  let rightward = r * SurfaceFunction.cos theta
  let forward = r * SurfaceFunction.sin theta
  let upward = h * SurfaceFunction.v
  let surfaceFunction = SurfaceFunction3d.rightwardForwardUpward rightward forward upward
  let domainCircle = Curve2d.circle (#centerPoint (Point2d.xy 0.5 0.5)) (#diameter (2 / 3))
  domain <- Tolerance.using 1e-9 (Region2d.boundedBy [domainCircle])
  let surface = Surface3d.parametric surfaceFunction domain
  let mesh = Surface3d.toMesh (Length.millimeters 2.0) surface
  IO.printLine ("Num faces: " <> Text.int (List.length (Mesh.faceIndices mesh)))
  let path = "executables/surface3d-meshing/mesh.stl"
  Stl.writeBinary path Convention3d.yUp Length.inMillimeters mesh
