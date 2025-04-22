module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d (Surface3d)
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d qualified as Vector2d

data GlobalSpace

type GlobalCoordinates = GlobalSpace @ Meters

top :: Surface3d GlobalCoordinates
top = do
  let theta = Angle.twoPi * SurfaceFunction.u
  let phi = Angle.quarterTurn * SurfaceFunction.v
  let r = Length.meter
  let x = r * SurfaceFunction.cos phi * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.cos phi * SurfaceFunction.sin theta
  let z = r * SurfaceFunction.sin phi
  Surface3d.parametric (SurfaceFunction3d.xyz x y z) Region2d.unitSquare

bottom :: Surface3d GlobalCoordinates
bottom = do
  let p1 = Point2d.meters 0.0 -2.0
  let p2 = Point2d.meters 1.0 0.0
  let v2 = Vector2d.meters 0.0 2.0
  let profile = Curve2d.hermite p1 [] p2 [v2] . SurfaceFunction.v
  let r = SurfaceFunction2d.xCoordinate profile
  let z = SurfaceFunction2d.yCoordinate profile
  let theta = Angle.twoPi * SurfaceFunction.u
  let x = r * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.sin theta
  Surface3d.parametric (SurfaceFunction3d.xyz x y z) Region2d.unitSquare

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  body <- Body3d.boundedBy [bottom, top]
  let constraints = NonEmpty.one (Mesh.maxSize (Length.centimeters 20.0))
  let mesh = Body3d.toMesh constraints body
  Stl.writeBinary "executables/raindrop/mesh.stl" Length.inMillimeters mesh
