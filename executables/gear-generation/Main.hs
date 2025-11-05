module Main (main) where

import Data.Text (Text)
import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude ((*.), type (@))
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result (Result)
import OpenSolid.Result qualified as Result
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance (Tolerance)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)
import OpenSolid.World3d qualified as World3d

gearBody :: Tolerance Meters => Int -> Result Text (Body3d (space @ Meters))
gearBody numTeeth = do
  let gearModule = Length.millimeters 1
  let holeDiameter = Length.millimeters 8
  let spurGear = SpurGear.metric (#numTeeth numTeeth) (#module gearModule)
  let outerProfile = SpurGear.profile spurGear
  let hole = Curve2d.circle (#centerPoint Point2d.origin) (#diameter holeDiameter)
  profile <- Result.try (Region2d.boundedBy (hole : outerProfile))
  let width = Length.millimeters 8
  Result.try (Body3d.extruded World3d.frontPlane profile (-0.5 *. width) (0.5 *. width))

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) do
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let writeGlb numTeeth = do
        timer <- Timer.start
        body <- IO.try (gearBody numTeeth)
        let glbPath = "executables/gear-generation/gear" <> Text.int numTeeth <> ".glb"
        let material = PbrMaterial.iron (#roughness 0.3)
        let model = Model3d.bodyWith [Model3d.pbrMaterial material] body
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
