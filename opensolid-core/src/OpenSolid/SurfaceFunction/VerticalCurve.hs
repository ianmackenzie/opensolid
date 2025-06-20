module OpenSolid.SurfaceFunction.VerticalCurve
  ( MonotonicSpace
  , new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d (Direction2d))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.ImplicitCurveBounds (ImplicitCurveBounds)
import OpenSolid.SurfaceFunction.ImplicitCurveBounds qualified as ImplicitCurveBounds
import OpenSolid.SurfaceFunction.Internal qualified as Internal
import OpenSolid.SurfaceParameter (SurfaceParameter (U))
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data MonotonicSpace

data Monotonicity
  = Monotonic
  | MonotonicIn (Frame2d UvCoordinates (Defines MonotonicSpace))
  | NotMonotonic
  deriving (Eq, Show)

implicitCurveBounds :: NonEmpty UvBounds -> ImplicitCurveBounds
implicitCurveBounds boxes =
  ImplicitCurveBounds.build (NonEmpty.map (\(Bounds2d u v) -> (v, u)) boxes)

new ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
new derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes NotMonotonic []

monotonic ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
monotonic derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes Monotonic []

bounded ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Frame2d UvCoordinates defines ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
bounded derivatives dudv vStart vEnd boxes monotonicFrame boundingAxes = do
  let monotonicity = MonotonicIn (Frame2d.coerce monotonicFrame)
  verticalCurve derivatives dudv vStart vEnd boxes monotonicity boundingAxes

verticalCurve ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Monotonicity ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
verticalCurve f dudv vStart vEnd boxes monotonicity boundingAxes = do
  let fu = SurfaceFunction.derivative U f
  let bounds = implicitCurveBounds boxes
  let clampedUBounds vValue =
        List.foldl (clamp vValue) (ImplicitCurveBounds.evaluate bounds vValue) boundingAxes
  let solveForU =
        case (f.compiled, fu.compiled) of
          (CompiledFunction.Concrete fExpr, CompiledFunction.Concrete fuExpr) ->
            \vValue -> Expression.solveMonotonicSurfaceU fExpr fuExpr (clampedUBounds vValue) vValue
          _ -> \vValue -> Internal.solveForU f fu (clampedUBounds vValue) vValue
  let evaluate tValue = do
        let vValue = Float.interpolateFrom vStart vEnd tValue
        let uValue = solveForU vValue
        Point2d uValue vValue
  let evaluateBounds tBounds = do
        let Bounds t1 t2 = tBounds
        let v1 = Float.interpolateFrom vStart vEnd t1
        let v2 = Float.interpolateFrom vStart vEnd t2
        let u1 = solveForU v1
        let u2 = solveForU v2
        case monotonicity of
          Monotonic -> Bounds2d (Bounds u1 u2) (Bounds v1 v2)
          MonotonicIn frame -> do
            let p1 = Point2d u1 v1
            let p2 = Point2d u2 v2
            Bounds2d.hull2 (Point2d.relativeTo frame p1) (Point2d.relativeTo frame p2)
              |> Bounds2d.placeIn frame
          NotMonotonic -> do
            let uBounds = ImplicitCurveBounds.evaluateBounds bounds (Bounds v1 v2)
            let slopeBounds = SurfaceFunction.evaluateBounds dudv (Bounds2d uBounds (Bounds v1 v2))
            let segmentUBounds = Internal.curveBoundsAt v1 v2 u1 u2 slopeBounds
            Bounds2d segmentUBounds (Bounds v1 v2)
  let derivative self = do
        let deltaV = vEnd - vStart
        let dvdt = Curve.constant deltaV
        let dudt = dvdt * dudv . self
        VectorCurve2d.xy dudt dvdt
  Curve2d.recursive (CompiledFunction.abstract evaluate evaluateBounds) derivative

clamp :: Float -> Bounds Unitless -> Axis2d UvCoordinates -> Bounds Unitless
clamp v (Bounds uLow uHigh) axis = do
  let Point2d u0 v0 = Axis2d.originPoint axis
  let Direction2d du dv = Axis2d.direction axis
  let u = u0 + (v - v0) * du / dv
  if
    | dv > 0.0 -> Bounds uLow (Qty.min uHigh u)
    | dv < 0.0 -> Bounds (Qty.max uLow u) uHigh
    | otherwise -> Bounds uLow uHigh
