module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.IO qualified as IO
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using Length.defaultTolerance do
  let axes = [Axis2D.y, -Axis2D.y]
  let sweptAngles = [Angle.degrees 45.0, Angle.degrees -45.0]
  let interval = Interval (Length.centimeters 5.0) (Length.centimeters 10.0)
  leftRegion <- Region2D.rectangle (Bounds2D -interval interval) & Result.orFail
  rightRegion <- Region2D.rectangle (Bounds2D interval interval) & Result.orFail
  let profiles = [leftRegion, rightRegion]
  bodies <-
    Result.all
      [ Body3D.revolved World3D.frontPlane profile axis sweptAngle
      | profile <- profiles
      , axis <- axes
      , sweptAngle <- sweptAngles
      ]
      & Result.orFail
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  IO.forEachWithIndex bodies $ \index body -> do
    let path = "executables/revolved-normals/mesh" <> Text.int (index + 1) <> ".stl"
    Stl.writeBinary path Convention3D.yUp Length.inMillimeters (Body3D.toPointMesh resolution body)
