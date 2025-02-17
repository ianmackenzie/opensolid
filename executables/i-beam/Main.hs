module Main (main) where

import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Scene3d qualified as Scene3d
import OpenSolid.Scene3d.Material qualified as Material
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using Length.nanometer IO.do
  let length = Length.centimeters 30.0
  let width = Length.centimeters 10.0
  let height = Length.centimeters 15.0
  let thickness = Length.centimeters 2.0
  let filletRadius = Length.millimeters 5.0
  let p1 = Point2d.x (0.5 * thickness)
  let p2 = Point2d.xy (0.5 * thickness) (0.5 * height - thickness)
  let p3 = Point2d.xy (0.5 * width) (0.5 * height - thickness)
  let p4 = Point2d.xy (0.5 * width) (0.5 * height)
  let p5 = Point2d.y (0.5 * height)
  let fillet = Curve2d.cornerArc p2 Direction2d.y Direction2d.x filletRadius
  let topRightCurves =
        [ Curve2d.line p1 (Curve2d.startPoint fillet)
        , fillet
        , Curve2d.line (Curve2d.endPoint fillet) p3
        , Curve2d.line p3 p4
        , Curve2d.line p4 p5
        ]
  let topCurves = topRightCurves + List.map (Curve2d.mirrorAcross Axis2d.y) topRightCurves
  let allCurves = topCurves + List.map (Curve2d.mirrorAcross Axis2d.x) topCurves
  profile <- Region2d.boundedBy allCurves
  body <- Body3d.extruded Plane3d.yz profile (Range (-0.5 * length) (0.5 * length))
  let meshConstraints = NonEmpty.one (Mesh.maxError (Length.millimeters 1.0))
  let mesh = Body3d.toMesh meshConstraints body
  let color = Color.rgb 0.913 0.921 0.925
  let material = Material.metal color 0.3
  let entity = Scene3d.mesh mesh material
  Scene3d.writeGlb "executables/i-beam/mesh.glb" Plane3d.xy [entity]
