module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvRegion qualified as UvRegion
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using Length.nanometer do
  let majorRadius = Length.meter
  let k = Length.meters 2.0
  let minorRadius =
        Curve1D.hermite Length.zero [k] Length.zero [-k] `compose` SurfaceFunction1D.u
  let theta = Curve1D.interpolateFrom (Angle.degrees 45.0) (Angle.degrees 315.0) `compose` SurfaceFunction1D.u
  let phi = Angle.twoPi * SurfaceFunction1D.v
  let r = majorRadius + minorRadius * SurfaceFunction1D.cos phi
  let surfaceFunction =
        World3D.originPoint
          + r * SurfaceFunction1D.cos theta * World3D.rightwardDirection
          + r * SurfaceFunction1D.sin theta * World3D.forwardDirection
          + minorRadius * SurfaceFunction1D.sin phi * World3D.upwardDirection
  let surface = Surface3D.parametric surfaceFunction UvRegion.unitSquare
  body <- Result.orFail (Body3D.boundedBy [surface])
  let resolution = Resolution.maxSize (Length.centimeters 20.0)
  let mesh = Body3D.toPointMesh resolution body
  Stl.writeBinary "executables/croissant/mesh.stl" Convention3D.yUp Length.inMillimeters mesh
