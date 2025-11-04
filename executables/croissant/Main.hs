module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve qualified as Curve
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Stl qualified as Stl
import OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Syntax (compose, negative, (.*.), (.+.))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d

main :: IO ()
main = Tolerance.using Length.nanometer do
  let majorRadius = Length.meter
  let k = Length.meters 2
  let minorRadius =
        Curve.hermite Length.zero [k] Length.zero [negative k] `compose` SurfaceFunction.u
  let theta = Curve.line (Angle.degrees 45) (Angle.degrees 315) `compose` SurfaceFunction.u
  let phi = Angle.twoPi .*. SurfaceFunction.v
  let r = majorRadius .+. minorRadius .*. SurfaceFunction.cos phi
  let surfaceFunction =
        World3d.originPoint
          .+. r .*. SurfaceFunction.cos theta .*. World3d.rightwardDirection
          .+. r .*. SurfaceFunction.sin theta .*. World3d.forwardDirection
          .+. minorRadius .*. SurfaceFunction.sin phi .*. World3d.upwardDirection
  let surface = Surface3d.parametric surfaceFunction Region2d.unitSquare
  body <- IO.try (Body3d.boundedBy [surface])
  let resolution = Resolution.maxSize (Length.centimeters 20)
  let mesh = Body3d.toMesh resolution body
  Stl.writeBinary "executables/croissant/mesh.stl" Convention3d.yUp Length.inMillimeters mesh
