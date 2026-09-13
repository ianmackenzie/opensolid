module Tests.Surface3D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import Test (Test)
import Test qualified
import Tests.Matching (matching)
import Tests.Random qualified as Random

spherePatch :: Tolerance Meters => Generator (Surface3D space)
spherePatch = do
  sketchPlane <- Random.plane3D
  let radius = Length.centimeters 10.0
  let profile = Curve2D.arcFrom (Point2D.x radius) (Point2D.y radius) Angle.quarterTurn
  let surface = Surface3D.revolved sketchPlane profile Axis2D.y Angle.quarterTurn
  Random.return surface

isOnPole :: UvPoint -> Bool
isOnPole (UvPoint u _) = unitless (u ~= 1.0)

nonPoleUvPoint :: Generator UvPoint
nonPoleUvPoint = UvPoint.random & Random.filter (not . isOnPole)

tests :: List Test
tests =
  [ findPoint
  ]

findPoint :: Test
findPoint =
  Test.group "findPoint" $
    [ findPole
    , findInteriorPoint
    ]

findPole :: Test
findPole = Test.check 100 "findPole" do
  surface <- Test.generate spherePatch
  let function = Surface3D.function surface
  let point = SurfaceFunction3D.pointAt (UvPoint 1.0 0.0) function
  solutions <- Surface3D.findPoint point surface ?? fail
  case solutions of
    [SurfacePoint3D.Pole (Nondegenerate poleCurve) _] -> do
      let expectedPoleCurve = Curve2D.lineFrom (UvPoint 1.0 0.0) (UvPoint 1.0 1.0)
      Test.expect (matching poleCurve expectedPoleCurve)
    _ ->
      Test.fail "Expected a single solution"
        & Test.output "solutions" solutions

findInteriorPoint :: Test
findInteriorPoint = Test.check 100 "findInterior" do
  surface <- Test.generate spherePatch
  let function = Surface3D.function surface
  uvPoint <- Test.generate nonPoleUvPoint
  let point = SurfaceFunction3D.pointAt uvPoint function
  solutions <- Surface3D.findPoint point surface ?? fail
  case solutions of
    [SurfacePoint3D.Point solution _] ->
      Test.expect (matching solution uvPoint)
    _ ->
      Test.fail "Expected a single solution"
        & Test.output "solutions" solutions
