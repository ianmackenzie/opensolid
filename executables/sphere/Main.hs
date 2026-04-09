module Main (main) where

import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using Tolerance.length do
  body <- Body3D.sphere (#centerPoint World3D.originPoint) (#diameter (Length.centimeters 10.0)) & Result.orFail
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  Stl.writeText "executables/sphere/mesh.stl" Convention3D.yUp Length.inMillimeters (Body3D.toPointMesh resolution body)
