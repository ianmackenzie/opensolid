module Main (main) where

import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Length qualified as Length
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Try qualified as Try

gearBody :: Tolerance Meters => Int -> Result Text (Body3d (space @ Meters))
gearBody numTeeth = Try.do
  let world = Frame3d.world
  let gearModule = Length.millimeters 1.0
  let holeDiameter = Length.millimeters 8.0
  let spurGear = SpurGear.metric (#numTeeth numTeeth, #module gearModule)
  let outerProfile = SpurGear.profile spurGear
  let hole = Curve2d.circle (#centerPoint Point2d.origin, #diameter holeDiameter)
  profile <- Region2d.boundedBy (hole : outerProfile)
  let width = Length.millimeters 8.0
  let extrusionBounds = Bounds.symmetric (#width width)
  Body3d.extruded world.frontPlane profile extrusionBounds

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) IO.do
  let resolution = Resolution.maxError (Length.millimeters 0.1)
  let writeGlb numTeeth = IO.do
        timer <- Timer.start
        body <- gearBody numTeeth
        let glbPath = "executables/gear-generation/gear" <> Text.int numTeeth <> ".glb"
        let material = PbrMaterial.iron (#roughness 0.3)
        let model = Model3d.bodyWith [Model3d.pbrMaterial material] body
        Gltf.writeBinary glbPath model resolution
        elapsed <- Timer.elapsed timer
        let elapsedText = Text.float (Duration.inSeconds elapsed) <> "s"
        IO.printLine ("Elapsed for " <> Text.int numTeeth <> " teeth: " <> elapsedText)
  overallTimer <- Timer.start
  let toothCounts = [32, 48, 80, 64, 96, 112, 128]
  -- IO.forEach toothCounts writeGlb
  IO.Parallel.forEach toothCounts writeGlb
  overallElapsed <- Timer.elapsed overallTimer
  IO.printLine ("Overall elapsed time: " <> Text.float (Duration.inSeconds overallElapsed) <> "s")
