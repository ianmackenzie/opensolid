{-# LANGUAGE UnboxedTuples #-}

module Tests.NewtonRaphson (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import Test (Test)
import Test qualified

data Space

tests :: List Test
tests =
  Tolerance.using 1e-9 $
    [ quadratic1d
    , arc2d
    , simpleSurface2d
    ]

quadratic1d :: Tolerance Unitless => Test
quadratic1d =
  curve1d "Quadratic" (Curve.squared Curve.t .- 2) 1 (Number.sqrt 2)

arc2d :: Tolerance Unitless => Test
arc2d = do
  let arc =
        Curve2d.polarArc
          (Named @"centerPoint" Point2D.origin)
          (Named @"radius" 1)
          (Named @"startAngle" Angle.zero)
          (Named @"endAngle" Angle.pi)
  let point = Point2D (Number.sqrt 2 ./ 2) (Number.sqrt 2 ./ 2)
  curve2d "Arc" (arc .-. point) 0.5 0.25

simpleSurface2d :: Tolerance Unitless => Test
simpleSurface2d = do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let x = SurfaceFunction.squared u .- 2
  let y = v .- 1
  let surface = VectorSurfaceFunction2d.xy x y
  surface2d "Simple 2D surface" surface (UvPoint 1 0) (UvPoint (Number.sqrt 2) 1)

curve1d :: Tolerance Unitless => Text -> Curve Unitless -> Number -> Number -> Test
curve1d name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" do
        let tSolution = NewtonRaphson.curve1d curve t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate# t# = let !(Quantity# x#) = Curve.evaluate curve (Quantity# t#) in x#
        let evaluateDerivative# t# =
              let !(Quantity# y'##) = Curve.evaluate curve.derivative (Quantity# t#) in y'##
        let tSolution = NewtonRaphson.curve1d# evaluate# evaluateDerivative# t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    ]

curve2d :: Tolerance Unitless => Text -> VectorCurve2d Unitless Space -> Number -> Number -> Test
curve2d name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" do
        let tSolution = NewtonRaphson.curve2d curve t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate# t# = do
              let vector = VectorCurve2d.evaluate curve (Quantity# t#)
              let !(Vector2D (Quantity# x#) (Quantity# y#)) = vector
              (# x#, y# #)
        let evaluateDerivative# t# = do
              let vector = VectorCurve2d.evaluate (VectorCurve2d.derivative curve) (Quantity# t#)
              let !(Vector2D (Quantity# x#) (Quantity# y#)) = vector
              (# x#, y# #)
        let tSolution = NewtonRaphson.curve2d# evaluate# evaluateDerivative# t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    ]

surface2d ::
  Tolerance Unitless =>
  Text ->
  VectorSurfaceFunction2d Unitless Space ->
  UvPoint ->
  UvPoint ->
  Test
surface2d name surface uv0 uvExpected =
  Test.verify name do
    let uvSolution = NewtonRaphson.surface2d surface uv0
    Test.expect (uvSolution ~= uvExpected)
      & Test.output "Expected solution" uvExpected
      & Test.output "Actual solution" uvSolution
