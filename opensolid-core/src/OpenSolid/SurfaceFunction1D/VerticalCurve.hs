module OpenSolid.SurfaceFunction1D.VerticalCurve
  ( new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.ImplicitCurveRange (ImplicitCurveRange)
import OpenSolid.SurfaceFunction1D.ImplicitCurveRange qualified as ImplicitCurveRange
import OpenSolid.SurfaceFunction1D.Internal qualified as Internal
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

data Monotonicity
  = Monotonic
  | MonotonicIn (Frame2D Unitless)
  | NotMonotonic
  deriving (Eq, Show)

implicitCurveRange :: NonEmpty UvBounds -> ImplicitCurveRange
implicitCurveRange boxes =
  ImplicitCurveRange.build (NonEmpty.map (\(Bounds2D u v) -> (v, u)) boxes)

new ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless
new derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes NotMonotonic []

monotonic ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless
monotonic derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes Monotonic []

bounded ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2D Unitless ->
  List (Axis2D Unitless) ->
  Curve2D Unitless
bounded derivatives dudv vStart vEnd boxes monotonicFrame boundingAxes = do
  let monotonicity = MonotonicIn (Frame2D.coerce monotonicFrame)
  verticalCurve derivatives dudv vStart vEnd boxes monotonicity boundingAxes

verticalCurve ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Monotonicity ->
  List (Axis2D Unitless) ->
  Curve2D Unitless
verticalCurve f dudv vStart vEnd boxes monotonicity boundingAxes = do
  let curveRange = implicitCurveRange boxes
  let clampedURange vValue =
        List.foldl (clamp vValue) (ImplicitCurveRange.at vValue curveRange) boundingAxes
  let solveForU =
        case (f.compiled, f.du.compiled) of
          (CompiledFunction.Concrete fExpr, CompiledFunction.Concrete fuExpr) ->
            \vValue -> Expression.solveMonotonicSurfaceU fExpr fuExpr (clampedURange vValue) vValue
          _ -> \vValue -> Internal.solveForU f f.du (clampedURange vValue) vValue
  let value tValue = do
        let vValue = Number.interpolateFrom vStart vEnd tValue
        let uValue = solveForU vValue
        UvPoint uValue vValue
  let range (Interval t1 t2) = do
        let v1 = Number.interpolateFrom vStart vEnd t1
        let v2 = Number.interpolateFrom vStart vEnd t2
        let u1 = solveForU v1
        let u2 = solveForU v2
        case monotonicity of
          Monotonic -> Bounds2D (Interval u1 u2) (Interval v1 v2)
          MonotonicIn frame -> do
            let p1 = UvPoint u1 v1
            let p2 = UvPoint u2 v2
            Bounds2D.hull2 (Point2D.relativeTo frame p1) (Point2D.relativeTo frame p2)
              & Bounds2D.placeIn frame
          NotMonotonic -> do
            let uRange = ImplicitCurveRange.over (Interval v1 v2) curveRange
            let slopeRange = SurfaceFunction1D.range dudv (Bounds2D uRange (Interval v1 v2))
            let segmentURange = Internal.curveRangeAt v1 v2 u1 u2 slopeRange
            Bounds2D segmentURange (Interval v1 v2)
  recursive \self -> do
    let dvdt = Curve1D.constant (vEnd - vStart)
    let dudt = dvdt * dudv . self
    Curve2D.new (CompiledFunction.abstract value range) (VectorCurve2D.xy dudt dvdt)

clamp :: Number -> Interval Unitless -> Axis2D Unitless -> Interval Unitless
clamp v (Interval uLow uHigh) axis = do
  let UvPoint u0 v0 = Axis2D.originPoint axis
  let Direction2D du dv = Axis2D.direction axis
  let u = u0 + (v - v0) * du / dv
  if
    | dv > 0.0 -> Interval uLow (min uHigh u)
    | dv < 0.0 -> Interval (max uLow u) uHigh
    | otherwise -> Interval uLow uHigh
