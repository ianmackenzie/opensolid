module Main (main) where

import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Color qualified as Color
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length qualified as Length
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Prelude
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d
import Prelude hiding (length)

main :: IO ()
main = Tolerance.using Length.nanometer do
  let length = Length.centimeters 30
  let width = Length.centimeters 10
  let height = Length.centimeters 15
  let thickness = Length.centimeters 2
  let filletRadius = Length.millimeters 5
  let webTopRight = Point2D (0.5 *. thickness) (0.5 *. height .-. thickness)
  let topFlangeBottomRight = Point2D (0.5 *. width) (0.5 *. height .-. thickness)
  let topFlangeTopRight = Point2D (0.5 *. width) (0.5 *. height)
  let topRightPoints = NonEmpty.three webTopRight topFlangeBottomRight topFlangeTopRight
  let topLeftPoints = NonEmpty.reverseMap (Point2D.mirrorAcross Axis2d.y) topRightPoints
  let topPoints = topRightPoints <> topLeftPoints
  let bottomPoints = NonEmpty.reverseMap (Point2D.mirrorAcross Axis2d.x) topPoints
  let allPoints = topPoints <> bottomPoints
  let filletPoints =
        [ webTopRight
        , webTopRight & Point2D.mirrorAcross Axis2d.y
        , webTopRight & Point2D.mirrorAcross Axis2d.x
        , webTopRight & Point2D.mirrorAcross Axis2d.y & Point2D.mirrorAcross Axis2d.x
        ]
  baseProfile <- Result.orFail (Region2d.polygon (Polygon2d allPoints))
  profile <- Result.orFail (Region2d.fillet filletPoints (#radius filletRadius) baseProfile)
  body <- Result.orFail (Body3d.extruded World3d.frontPlane profile (-0.5 *. length) (0.5 *. length))
  let material = PbrMaterial.metal (Color.rgb1 0.913 0.921 0.925) (#roughness 0.3)
  let model = Model3d.bodyWith [Model3d.pbrMaterial material] body
  let resolution = Resolution.maxError (Length.millimeters 1)
  Gltf.writeBinary "executables/i-beam/mesh.glb" model resolution
