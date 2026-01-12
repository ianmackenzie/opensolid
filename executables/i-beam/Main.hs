module Main (main) where

import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Color qualified as Color
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length qualified as Length
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polygon2D (Polygon2D (Polygon2D))
import OpenSolid.Prelude
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3D qualified as World3D
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
  let topLeftPoints = NonEmpty.reverseMap (Point2D.mirrorAcross Axis2D.y) topRightPoints
  let topPoints = topRightPoints <> topLeftPoints
  let bottomPoints = NonEmpty.reverseMap (Point2D.mirrorAcross Axis2D.x) topPoints
  let allPoints = topPoints <> bottomPoints
  let filletPoints =
        [ webTopRight
        , webTopRight & Point2D.mirrorAcross Axis2D.y
        , webTopRight & Point2D.mirrorAcross Axis2D.x
        , webTopRight & Point2D.mirrorAcross Axis2D.y & Point2D.mirrorAcross Axis2D.x
        ]
  baseProfile <- Result.orFail (Region2D.polygon (Polygon2D allPoints))
  profile <- Result.orFail (Region2D.fillet filletPoints (#radius filletRadius) baseProfile)
  body <- Result.orFail (Body3D.extruded World3D.frontPlane profile (-0.5 *. length) (0.5 *. length))
  let material = PbrMaterial.metal (Color.rgb1 0.913 0.921 0.925) (#roughness 0.3)
  let model = Model3D.bodyWith [Model3D.pbrMaterial material] body
  let resolution = Resolution.maxError (Length.millimeters 1)
  Gltf.writeBinary "executables/i-beam/mesh.glb" model resolution
