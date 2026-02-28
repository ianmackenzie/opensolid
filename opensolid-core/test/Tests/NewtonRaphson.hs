{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tests.NewtonRaphson (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Length qualified as Length
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D
import OpenSolid.World3D qualified as World3D
import Test (Test)
import Test qualified

data Space

tests :: List Test
tests =
  Tolerance.using Length.nanometer $
    [ quadratic1D
    , arc2D
    , simpleSurface2D
    , pointOnSphere3D
    ]

quadratic1D :: Test
quadratic1D =
  curve1D "Quadratic" (Curve1D.squared Curve1D.t - 2.0) 1.0 (Number.sqrt 2.0)

arc2D :: Test
arc2D = do
  let arc =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius 1.0)
          (#startAngle Angle.zero)
          (#endAngle Angle.pi)
  let point = Point2D (Number.sqrt 2.0 / 2.0) (Number.sqrt 2.0 / 2.0)
  curve2D "Arc" (arc - point) 0.5 0.25

simpleSurface2D :: Test
simpleSurface2D = Test.verify "Simple 2D surface" Test.do
  let u = SurfaceFunction1D.u
  let v = SurfaceFunction1D.v
  let x = SurfaceFunction1D.squared u - 2.0
  let y = v - 1.0
  let surface = VectorSurfaceFunction2D.xy x y
  surface2D surface (UvPoint 1.0 0.0) (UvPoint (Number.sqrt 2.0) 1.0)

pointOnSphere3D :: Tolerance Meters => Test
pointOnSphere3D = Test.verify "Point on sphere" Test.do
  let radius = Length.meters 1.0
  let profileCurve =
        Curve2D.polarArc
          (#centerPoint Point2D.origin)
          (#radius radius)
          (#startAngle Angle.zero)
          (#endAngle Angle.halfPi)
  surface <- Surface3D.revolved World3D.rightPlane profileCurve Axis2D.y Angle.twoPi
  let point =
        Point2D.polar radius (Angle.degrees 45.0)
          & Point2D.placeOn World3D.rightPlane
          & Point3D.rotateAround World3D.upwardAxis (Angle.degrees 45.0)
  let displacement = point - Surface3D.function surface
  surface3D displacement (UvPoint 0.1 0.4) (UvPoint 0.125 0.5)

curve1D :: Text -> Curve1D Unitless -> Number -> Number -> Test
curve1D name curve t0 tExpected =
  Test.verify name do
    let tSolution = Curve1D.newtonRaphson curve t0
    Test.expect (Tolerance.using Tolerance.unitless (tSolution ~= tExpected))
      & Test.output "Expected solution" tExpected
      & Test.output "Actual solution" tSolution

curve2D :: Text -> VectorCurve2D Unitless Space -> Number -> Number -> Test
curve2D name curve t0 tExpected =
  Test.verify name do
    let tSolution = VectorCurve2D.newtonRaphson curve t0
    Test.expect (Tolerance.using Tolerance.unitless (tSolution ~= tExpected))
      & Test.output "Expected solution" tExpected
      & Test.output "Actual solution" tSolution

surface2D ::
  VectorSurfaceFunction2D Unitless Space ->
  UvPoint ->
  UvPoint ->
  Test.Expectation
surface2D surface uv0 uvExpected = do
  let uvSolution = VectorSurfaceFunction2D.newtonRaphson surface uv0
  Test.expect (Tolerance.using Tolerance.unitless (uvSolution ~= uvExpected))
    & Test.output "Expected solution" uvExpected
    & Test.output "Actual solution" uvSolution

surface3D ::
  Tolerance Meters =>
  VectorSurfaceFunction3D Meters Space ->
  UvPoint ->
  UvPoint ->
  Test.Expectation
surface3D surface uv0 uvExpected = do
  let uvSolution = VectorSurfaceFunction3D.newtonRaphson surface uv0
  Test.expect (Tolerance.using Tolerance.unitless (uvSolution ~= uvExpected))
    & Test.output "Expected solution" uvExpected
    & Test.output "Actual solution" uvSolution
