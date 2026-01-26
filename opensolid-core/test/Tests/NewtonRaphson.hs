{-# LANGUAGE UnboxedTuples #-}

module Tests.NewtonRaphson (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import Test (Test)
import Test qualified

data Space

tests :: List Test
tests =
  Tolerance.using 1e-9 $
    [ quadratic1D
    , arc2D
    , simpleSurface2D
    ]

quadratic1D :: Tolerance Unitless => Test
quadratic1D =
  curve1D "Quadratic" (Curve1D.squared Curve1D.t .- 2) 1 (Number.sqrt 2)

arc2D :: Tolerance Unitless => Test
arc2D = do
  let arc =
        Curve2D.polarArc
          (Named @"centerPoint" Point2D.origin)
          (Named @"radius" 1)
          (Named @"startAngle" Angle.zero)
          (Named @"endAngle" Angle.pi)
  let point = Point2D (Number.sqrt 2 ./ 2) (Number.sqrt 2 ./ 2)
  curve2D "Arc" (arc .-. point) 0.5 0.25

simpleSurface2D :: Tolerance Unitless => Test
simpleSurface2D = do
  let u = SurfaceFunction1D.u
  let v = SurfaceFunction1D.v
  let x = SurfaceFunction1D.squared u .- 2
  let y = v .- 1
  let surface = VectorSurfaceFunction2D.xy x y
  surface2D "Simple 2D surface" surface (UvPoint 1 0) (UvPoint (Number.sqrt 2) 1)

curve1D :: Tolerance Unitless => Text -> Curve1D Unitless -> Number -> Number -> Test
curve1D name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" do
        let tSolution = NewtonRaphson.curve1D curve t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate# t# = let !(Quantity# x#) = Curve1D.evaluate curve (Quantity# t#) in x#
        let evaluateDerivative# t# = do
              let !(Quantity# y'##) = Curve1D.evaluate (Curve1D.derivative curve) (Quantity# t#)
              y'##
        let tSolution = NewtonRaphson.curve1D# evaluate# evaluateDerivative# t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    ]

curve2D :: Tolerance Unitless => Text -> VectorCurve2D Unitless Space -> Number -> Number -> Test
curve2D name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" do
        let tSolution = NewtonRaphson.curve2D curve t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate# t# = do
              let vector = VectorCurve2D.evaluate curve (Quantity# t#)
              let !(Vector2D (Quantity# x#) (Quantity# y#)) = vector
              (# x#, y# #)
        let evaluateDerivative# t# = do
              let vector = VectorCurve2D.evaluate (VectorCurve2D.derivative curve) (Quantity# t#)
              let !(Vector2D (Quantity# x#) (Quantity# y#)) = vector
              (# x#, y# #)
        let tSolution = NewtonRaphson.curve2D# evaluate# evaluateDerivative# t0
        Test.expect (tSolution ~= tExpected)
          & Test.output "Expected solution" tExpected
          & Test.output "Actual solution" tSolution
    ]

surface2D ::
  Tolerance Unitless =>
  Text ->
  VectorSurfaceFunction2D Unitless Space ->
  UvPoint ->
  UvPoint ->
  Test
surface2D name surface uv0 uvExpected =
  Test.verify name do
    let uvSolution = NewtonRaphson.surface2D surface uv0
    Test.expect (uvSolution ~= uvExpected)
      & Test.output "Expected solution" uvExpected
      & Test.output "Actual solution" uvSolution
