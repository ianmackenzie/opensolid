module Main (main) where

import OpenSolid.Angle qualified as Angle
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
  let x = r * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.sin theta
  let z = h * SurfaceFunction.v
  let surfaceFunction = SurfaceFunction3d.xyz x y z
  let domainCircle = Curve2d.circle (Point2d.xy 0.5 0.5) (1 / 3)
  domain <- Tolerance.using 1e-9 (Region2d.boundedBy [domainCircle])
  let surface = Surface3d.parametric surfaceFunction domain
  let mesh = Surface3d.toMesh (Length.millimeters 2.0) surface
  IO.printLine ("Num faces: " + Text.int (List.length (Mesh.faceIndices mesh)))
  Stl.writeBinary "executables/surface3d-meshing/mesh.stl" Length.inMillimeters mesh
