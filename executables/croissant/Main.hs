module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve qualified as Curve
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer $ IO.do
  let majorRadius = Length.meter
  let k = Length.meters 2.0
  let minorRadius = Curve.hermite (Length.zero, [k]) (Length.zero, [-k]) . SurfaceFunction.u
  let theta = Curve.line (Angle.degrees 45.0) (Angle.degrees 315.0) . SurfaceFunction.u
  let phi = Angle.twoPi * SurfaceFunction.v
  let r = majorRadius + minorRadius * SurfaceFunction.cos phi
  let x = r * SurfaceFunction.cos theta
  let y = r * SurfaceFunction.sin theta
  let z = minorRadius * SurfaceFunction.sin phi
  let surfaceFunction = SurfaceFunction3d.xyz x y z
  let surface = Surface3d.parametric surfaceFunction Region2d.unit
  body <- Body3d.boundedBy [surface]
  let constraints = NonEmpty.one (Mesh.maxSize (Length.centimeters 20.0))
  let mesh = Body3d.toMesh constraints body
  Stl.writeBinary "executables/croissant/mesh.stl" Length.inMillimeters mesh
