{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE ImplicitPrelude #-}

module Main (main) where

import OpenSolid qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D
import Prelude hiding (length)

main :: IO ()
main = Tolerance.using Length.nanometer do
  let radius = Length.meters 1.0
  let length = Length.meters 4.0
  let arc =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius radius)
          (#startAngle (Angle.degrees -45.0))
          (#endAngle (Angle.degrees 225.0))
  let line = Curve2D.lineFrom arc.endPoint arc.startPoint
  profile <- Result.orFail (Region2D.boundedBy [arc, line])
  let extrusionStart = OpenSolid.product (OpenSolid.number -0.5) length
  let extrusionEnd = OpenSolid.product (OpenSolid.number 0.5) length
  body <- Result.orFail (Body3D.extruded World3D.frontPlane profile extrusionStart extrusionEnd)
  let resolution = Resolution.maxSize (Length.centimeters 30.0)
  let mesh = Body3D.toPointMesh resolution body
  let outputPath = "executables/body3D-meshing/mesh.stl"
  Stl.writeBinary outputPath Convention3D.yUp Length.inMillimeters mesh
