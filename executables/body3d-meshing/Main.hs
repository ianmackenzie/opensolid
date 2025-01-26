module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d (Surface3d)
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceParameter (UvCoordinates)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)

data GlobalSpace

type GlobalCoordinates = GlobalSpace @ Meters

radius :: Length
radius = Length.meters 1.0

length :: Length
length = Length.meters 3.0

cylindricalSurface :: Surface3d GlobalCoordinates
cylindricalSurface = do
  let x = length * SurfaceFunction.u
  let theta = Angle.pi * SurfaceFunction.v
  let z = radius * SurfaceFunction.sin theta
  let y = -radius * SurfaceFunction.cos theta
  let surfaceFunction = SurfaceFunction3d.xyz x y z
  Surface3d.parametric surfaceFunction Region2d.unit

planarFunction ::
  Point3d GlobalCoordinates ->
  Direction3d GlobalSpace ->
  Direction3d GlobalSpace ->
  SurfaceFunction3d GlobalCoordinates
planarFunction p0 dx dy =
  SurfaceFunction3d.constant p0
    + Length.meter * dx * SurfaceFunction.u
    + Length.meter * dy * SurfaceFunction.v

makeSemicircle :: Result Text (Region2d UvCoordinates)
makeSemicircle = do
  let p1 = Point2d.x -1.0
  let p2 = Point2d.x 1.0
  Tolerance.using 1e-9 $
    Result.try $
      Region2d.boundedBy [Curve2d.line p1 p2, Curve2d.arc p2 p1 Angle.halfTurn]

makeStartCap :: Result Text (Surface3d GlobalCoordinates)
makeStartCap = Result.do
  let surfaceFunction = planarFunction Point3d.origin -Direction3d.y Direction3d.z
  domain <- makeSemicircle
  Success (Surface3d.parametric surfaceFunction domain)

makeEndCap :: Result Text (Surface3d GlobalCoordinates)
makeEndCap = Result.do
  let surfaceFunction = planarFunction (Point3d.x length) Direction3d.y Direction3d.z
  domain <- makeSemicircle
  Success (Surface3d.parametric surfaceFunction domain)

bottom :: Surface3d GlobalCoordinates
bottom = do
  let surfaceFunction =
        SurfaceFunction3d.constant (Point3d.y radius)
          + Direction3d.x * length * SurfaceFunction.u
          - 2.0 * Direction3d.y * radius * SurfaceFunction.v
  Surface3d.parametric surfaceFunction Region2d.unit

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  startCap <- makeStartCap
  endCap <- makeEndCap
  body <- Body3d.boundedBy [cylindricalSurface, startCap, endCap, bottom]
  let quality = Mesh.Quality{maxError = Qty.infinity, maxSize = Length.centimeters 20.0}
  let mesh = Body3d.toMesh quality body
  IO.writeFile "executables/body3d-meshing/mesh.stl" (Stl.text Length.inMillimeters mesh)
