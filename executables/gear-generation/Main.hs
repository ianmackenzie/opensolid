module Main (main) where

import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.IO.Parallel qualified as IO.Parallel
import OpenSolid.Labels (CenterPoint (CenterPoint), Diameter (Diameter))
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Result qualified as Result
import OpenSolid.Scene3d qualified as Scene3d
import OpenSolid.SpurGear (Module (Module), NumTeeth (NumTeeth))
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Stl qualified as Stl
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)

gearBody :: Tolerance Meters => Int -> Result Text (Body3d (space @ Meters))
gearBody numTeeth = Result.do
  let gearModule = Length.millimeters 1.0
  let holeDiameter = Length.millimeters 8.0
  let spurGear = SpurGear.metric (NumTeeth numTeeth) (Module gearModule)
  let outerProfile = SpurGear.profile spurGear
  let hole = Curve2d.circle (CenterPoint Point2d.origin) (Diameter holeDiameter)
  profile <- Result.try (Region2d.boundedBy (hole : outerProfile))
  let width = Length.millimeters 8.0
  Result.try (Body3d.extruded Plane3d.xy profile (Range (-width / 2.0) (width / 2.0)))

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) IO.do
  let meshConstraints = NonEmpty.one (Mesh.maxError (Length.millimeters 0.1))
  let writeGlb numTeeth = IO.do
        timer <- Timer.start
        body <- gearBody numTeeth
        let basePath = "executables/gear-generation/gear" <> Text.int numTeeth
        let glbPath = basePath <> ".glb"
        let stlPath = basePath <> ".stl"
        let roughness = 0.3
        let material = Scene3d.iron roughness
        let mesh = Body3d.toMesh meshConstraints body
        Scene3d.writeGlb glbPath Plane3d.xy [Scene3d.mesh material mesh]
        Stl.writeBinary stlPath Length.inMillimeters mesh
        elapsed <- Timer.elapsed timer
        IO.printLine ("Elapsed for " <> Text.int numTeeth <> " teeth: " <> Text.float (Duration.inSeconds elapsed) <> "s")
  overallTimer <- Timer.start
  let toothCounts = [32, 48, 80, 64, 96, 112, 128]
  -- IO.forEach toothCounts writeGlb
  IO.Parallel.forEach toothCounts writeGlb
  overallElapsed <- Timer.elapsed overallTimer
  IO.printLine ("Overall elapsed time: " <> Text.float (Duration.inSeconds overallElapsed) <> "s")
