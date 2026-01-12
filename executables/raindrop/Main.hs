module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length qualified as Length
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using Length.nanometer do
  let p1 = Point2D.meters 0 -2
  let p2 = Point2D.meters 1 0
  let v2 = Vector2D.meters 0 2
  let p3 = Point2D.meters 0 1
  profile <- Result.orFail do
    Region2D.boundedBy
      [ Curve2D.hermite p1 [] p2 [v2]
      , Curve2D.arcFrom p2 p3 Angle.quarterTurn
      , Curve2D.lineFrom p3 p1
      ]
  body <- Result.orFail (Body3D.revolved World3D.frontPlane profile Axis2D.y Angle.twoPi)
  let material = PbrMaterial.nonmetal Color.blue (#roughness 0.2)
  let model = Model3D.bodyWith [Model3D.pbrMaterial material] body
  let resolution = Resolution.maxSize (Length.centimeters 20)
  Gltf.writeBinary "executables/raindrop/mesh.glb" model resolution
