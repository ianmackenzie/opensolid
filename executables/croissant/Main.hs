module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let world = Frame3d.world
  let majorRadius = Length.meter
  let k = Length.meters 2.0
  let minorRadius = Curve.hermite Length.zero [k] Length.zero [-k] . SurfaceFunction.u
  let theta = Curve.line (Angle.degrees 45.0) (Angle.degrees 315.0) . SurfaceFunction.u
  let phi = Angle.twoPi * SurfaceFunction.v
  let r = majorRadius + minorRadius * SurfaceFunction.cos phi
  let surfaceFunction =
        world.originPoint
          + r * SurfaceFunction.cos theta * world.rightwardDirection
          + r * SurfaceFunction.sin theta * world.forwardDirection
          + minorRadius * SurfaceFunction.sin phi * world.upwardDirection
  let surface = Surface3d.parametric surfaceFunction Region2d.unitSquare
  body <- Body3d.boundedBy [surface]
  let resolution = Resolution.maxSize (Length.centimeters 20.0)
  let mesh = Body3d.toMesh resolution body
  Stl.writeBinary "executables/croissant/mesh.stl" Convention3d.yUp Length.inMillimeters mesh
