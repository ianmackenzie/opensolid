module Main (main) where

import OpenSolid.Body3D (Body3D)
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Duration qualified as Duration
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D

gearBody :: Tolerance Meters => Int -> Result Text (Body3D space)
gearBody numTeeth = do
  let gearModule = Length.millimeters 1
  let holeDiameter = Length.millimeters 8
  let spurGear = SpurGear.metric (#numTeeth numTeeth) (#module gearModule)
  let outerProfile = SpurGear.profile spurGear
  let hole = Curve2D.circle (Circle2D.withDiameter holeDiameter Point2D.origin)
  profile <- Result.orFail (Region2D.boundedBy (hole : outerProfile))
  let width = Length.millimeters 8
  Result.orFail (Body3D.extruded World3D.frontPlane profile (-0.5 *. width) (0.5 *. width))

main :: IO ()
main = Tolerance.using Length.nanometer do
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let writeGlb numTeeth = do
        timer <- Timer.start
        body <- Result.orFail (gearBody numTeeth)
        let glbPath = "executables/gear-generation/gear" <> Text.int numTeeth <> ".glb"
        let material = PbrMaterial.iron (#roughness 0.3)
        let model = Model3D.bodyWith [Model3D.pbrMaterial material] body
        Gltf.writeBinary glbPath model resolution
        elapsed <- Timer.elapsed timer
        let elapsedText = Text.number (Duration.inSeconds elapsed) <> "s"
        IO.printLine ("Elapsed for " <> Text.int numTeeth <> " teeth: " <> elapsedText)
  overallTimer <- Timer.start
  let toothCounts = [32, 48, 80, 64, 96, 112, 128]
  -- IO.forEach toothCounts writeGlb
  IO.Parallel.forEach toothCounts writeGlb
  overallElapsed <- Timer.elapsed overallTimer
  IO.printLine ("Overall elapsed time: " <> Text.number (Duration.inSeconds overallElapsed) <> "s")
