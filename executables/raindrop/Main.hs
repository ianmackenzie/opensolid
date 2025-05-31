module Main (main) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Scene3d qualified as Scene3d
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.World3d qualified as World3d

main :: IO ()
main = Tolerance.using (Length.meters 1e-9) IO.do
  let p1 = Point2d.meters 0.0 -2.0
  let p2 = Point2d.meters 1.0 0.0
  let v2 = Vector2d.meters 0.0 2.0
  let p3 = Point2d.meters 0.0 1.0
  profile <-
    Region2d.boundedBy
      [ Curve2d.hermite p1 [] p2 [v2]
      , Curve2d.arc p2 p3 Angle.quarterTurn
      , Curve2d.line p3 p1
      ]
  body <- Body3d.revolved World3d.frontPlane profile Axis2d.y Angle.twoPi
  let constraints = NonEmpty.one (Mesh.maxSize (Length.centimeters 20.0))
  let mesh = Body3d.toMesh constraints body
  let material = PbrMaterial.nonmetal Color.blue (#roughness 0.2)
  let scene = [Scene3d.mesh material mesh]
  Scene3d.writeGlb "executables/raindrop/mesh.glb" scene
