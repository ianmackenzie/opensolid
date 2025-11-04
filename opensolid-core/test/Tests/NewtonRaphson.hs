{-# LANGUAGE UnboxedTuples #-}

module Tests.NewtonRaphson (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Number qualified as Number
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import Test (Expectation, Test)
import Test qualified

data Space

tests :: List Test
tests =
  Tolerance.using 1e-9 $
    [ quadratic1d
    , quadraticDivergence1d
    , arc2d
    , simpleSurface2d
    ]

quadratic1d :: Tolerance Unitless => Test
quadratic1d =
  curve1d "Quadratic" (Curve.squared Curve.t - 2.0) 1.0 (Number.sqrt 2.0)

quadraticDivergence1d :: Tolerance Unitless => Test
quadraticDivergence1d =
  curveDivergence1d "Quadratic divergence" (Curve.squared Curve.t + 2.0) 1.0

arc2d :: Tolerance Unitless => Test
arc2d = do
  let arc =
        Curve2d.polarArc
          @ #centerPoint Point2d.origin
          @ #radius 1.0
          @ #startAngle Angle.zero
          @ #endAngle Angle.pi
  let point = Point2d (Number.sqrt 2.0 / 2.0) (Number.sqrt 2.0 / 2.0)
  curve2d "Arc" (arc - point) 0.5 0.25

simpleSurface2d :: Tolerance Unitless => Test
simpleSurface2d = do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  let x = SurfaceFunction.squared u - 2.0
  let y = v - 1.0
  let surface = VectorSurfaceFunction2d.xy x y
  surface2d "Simple 2D surface" surface (Point2d 1.0 0.0) (Point2d (Number.sqrt 2.0) 1.0)

expectedConvergence :: Expectation
expectedConvergence = Test.fail "Expected Newton-Raphson to converge but it did not"

expectedDivergence :: Expectation
expectedDivergence = Test.fail "Expected Newton-Raphson not to converge but it did"

curve1d :: Tolerance Unitless => Text -> Curve Unitless -> Number -> Number -> Test
curve1d name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" $
        case NewtonRaphson.curve1d (Curve.evaluate curve) (Curve.evaluate curve.derivative) t0 of
          Failure NewtonRaphson.Divergence -> expectedConvergence
          Success tSolution ->
            Test.expect (tSolution ~= tExpected)
              |> Test.output "Expected solution" tExpected
              |> Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate## t## = let !(Quantity## x##) = Curve.evaluate curve (Quantity## t##) in x##
        let evaluateDerivative## t## =
              let !(Quantity## y'##) = Curve.evaluate curve.derivative (Quantity## t##) in y'##
        case NewtonRaphson.curve1d## evaluate## evaluateDerivative## t0 of
          Failure NewtonRaphson.Divergence -> expectedConvergence
          Success tSolution ->
            Test.expect (tSolution ~= tExpected)
              |> Test.output "Expected solution" tExpected
              |> Test.output "Actual solution" tSolution
    ]

curveDivergence1d :: Tolerance Unitless => Text -> Curve Unitless -> Number -> Test
curveDivergence1d name curve t0 =
  Test.group name $
    [ Test.verify "Boxed" $
        case NewtonRaphson.curve1d (Curve.evaluate curve) (Curve.evaluate curve.derivative) t0 of
          Failure NewtonRaphson.Divergence -> Test.pass
          Success tSolution -> expectedDivergence |> Test.output "Solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate## t## = let !(Quantity## x##) = Curve.evaluate curve (Quantity## t##) in x##
        let evaluateDerivative## t## =
              let !(Quantity## y'##) = Curve.evaluate curve.derivative (Quantity## t##) in y'##
        case NewtonRaphson.curve1d## evaluate## evaluateDerivative## t0 of
          Failure NewtonRaphson.Divergence -> Test.pass
          Success tSolution -> expectedDivergence |> Test.output "Solution" tSolution
    ]

curve2d :: Tolerance Unitless => Text -> VectorCurve2d (Space @ Unitless) -> Number -> Number -> Test
curve2d name curve t0 tExpected =
  Test.group name $
    [ Test.verify "Boxed" $
        case NewtonRaphson.curve2d
          (VectorCurve2d.evaluate curve)
          (VectorCurve2d.evaluate curve.derivative)
          t0 of
          Failure NewtonRaphson.Divergence -> expectedConvergence
          Success tSolution ->
            Test.expect (tSolution ~= tExpected)
              |> Test.output "Expected solution" tExpected
              |> Test.output "Actual solution" tSolution
    , Test.verify "Unboxed" do
        let evaluate## t## = do
              let vector = VectorCurve2d.evaluate curve (Quantity## t##)
              let !(Vector2d (Quantity## x##) (Quantity## y##)) = vector
              (# x##, y## #)
        let evaluateDerivative## t## = do
              let vector = VectorCurve2d.evaluate curve.derivative (Quantity## t##)
              let !(Vector2d (Quantity## x##) (Quantity## y##)) = vector
              (# x##, y## #)
        case NewtonRaphson.curve2d## evaluate## evaluateDerivative## t0 of
          Failure NewtonRaphson.Divergence -> expectedConvergence
          Success tSolution ->
            Test.expect (tSolution ~= tExpected)
              |> Test.output "Expected solution" tExpected
              |> Test.output "Actual solution" tSolution
    ]

surface2d ::
  Tolerance Unitless =>
  Text ->
  VectorSurfaceFunction2d (Space @ Unitless) ->
  UvPoint ->
  UvPoint ->
  Test
surface2d name surface uv0 uvExpected =
  Test.verify name $
    case NewtonRaphson.surface2d
      (VectorSurfaceFunction2d.evaluate surface)
      (VectorSurfaceFunction2d.evaluate surface.du)
      (VectorSurfaceFunction2d.evaluate surface.dv)
      uv0 of
      Failure NewtonRaphson.Divergence -> expectedConvergence
      Success uvSolution ->
        Test.expect (uvSolution ~= uvExpected)
          |> Test.output "Expected solution" uvExpected
          |> Test.output "Actual solution" uvSolution
