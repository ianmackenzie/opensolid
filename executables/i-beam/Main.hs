module Main (main) where

import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let world = Frame3d.world
  let length = Length.centimeters 30.0
  let width = Length.centimeters 10.0
  let height = Length.centimeters 15.0
  let thickness = Length.centimeters 2.0
  let filletRadius = Length.millimeters 5.0
  let p1 = Point2d.x (0.5 * thickness)
  let p2 = Point2d (0.5 * thickness) (0.5 * height - thickness)
  let p3 = Point2d (0.5 * width) (0.5 * height - thickness)
  let p4 = Point2d (0.5 * width) (0.5 * height)
  let p5 = Point2d.y (0.5 * height)
  let fillet =
        Curve2d.cornerArc p2 do
          #incoming Direction2d.y
          #outgoing Direction2d.x
          #radius filletRadius
  let topRightCurves =
        [ Curve2d.line p1 fillet.startPoint
        , fillet
        , Curve2d.line fillet.endPoint p3
        , Curve2d.line p3 p4
        , Curve2d.line p4 p5
        ]
  let topCurves = topRightCurves <> List.map (Curve2d.mirrorAcross Axis2d.y) topRightCurves
  let allCurves = topCurves <> List.map (Curve2d.mirrorAcross Axis2d.x) topCurves
  profile <- Region2d.boundedBy allCurves
  body <- Body3d.extruded world.frontPlane profile (Bounds.symmetric (#width length))
  let material = PbrMaterial.metal (Color.rgbFloat 0.913 0.921 0.925) (#roughness 0.3)
  let model = Model3d.bodyWith [Model3d.pbrMaterial material] body
  let resolution = Resolution.maxError (Length.millimeters 1.0)
  Gltf.writeBinary "executables/i-beam/mesh.glb" model resolution
