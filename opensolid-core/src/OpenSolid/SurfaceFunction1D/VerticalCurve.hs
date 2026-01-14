module OpenSolid.SurfaceFunction1D.VerticalCurve
  ( MonotonicSpace
  , new
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
import OpenSolid.SurfaceFunction1D.ImplicitCurveBounds (ImplicitCurveBounds)
import OpenSolid.SurfaceFunction1D.ImplicitCurveBounds qualified as ImplicitCurveBounds
import OpenSolid.SurfaceFunction1D.Internal qualified as Internal
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

data MonotonicSpace

data Monotonicity
  = Monotonic
  | MonotonicIn (Frame2D Unitless UvSpace MonotonicSpace)
  | NotMonotonic
  deriving (Eq, Show)

implicitCurveBounds :: NonEmpty UvBounds -> ImplicitCurveBounds
implicitCurveBounds boxes =
  ImplicitCurveBounds.build (NonEmpty.map (\(Bounds2D u v) -> (v, u)) boxes)

new ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
new derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes NotMonotonic []

monotonic ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
monotonic derivatives dudv vStart vEnd boxes =
  verticalCurve derivatives dudv vStart vEnd boxes Monotonic []

bounded ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2D Unitless UvSpace local ->
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
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
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
verticalCurve f dudv vStart vEnd boxes monotonicity boundingAxes = do
  let bounds = implicitCurveBounds boxes
  let clampedUBounds vValue =
        List.foldl (clamp vValue) (ImplicitCurveBounds.evaluate bounds vValue) boundingAxes
  let solveForU =
        case (f.compiled, f.du.compiled) of
          (CompiledFunction.Concrete fExpr, CompiledFunction.Concrete fuExpr) ->
            \vValue -> Expression.solveMonotonicSurfaceU fExpr fuExpr (clampedUBounds vValue) vValue
          _ -> \vValue -> Internal.solveForU f f.du (clampedUBounds vValue) vValue
  let evaluate tValue = do
        let vValue = Number.interpolateFrom vStart vEnd tValue
        let uValue = solveForU vValue
        UvPoint uValue vValue
  let evaluateBounds tBounds = do
        let Interval t1 t2 = tBounds
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
            let uBounds = ImplicitCurveBounds.evaluateBounds bounds (Interval v1 v2)
            let slopeBounds = SurfaceFunction1D.evaluateBounds dudv (Bounds2D uBounds (Interval v1 v2))
            let segmentUBounds = Internal.curveBoundsAt v1 v2 u1 u2 slopeBounds
            Bounds2D segmentUBounds (Interval v1 v2)
  let derivative self = do
        let deltaV = vEnd .-. vStart
        let dvdt = Curve1D.constant deltaV
        let dudt = dvdt .*. dudv `compose` self
        VectorCurve2D.xy dudt dvdt
  Curve2D.recursive (CompiledFunction.abstract evaluate evaluateBounds) derivative

clamp :: Number -> Interval Unitless -> Axis2D Unitless UvSpace -> Interval Unitless
clamp v (Interval uLow uHigh) axis = do
  let UvPoint u0 v0 = Axis2D.originPoint axis
  let Direction2D du dv = Axis2D.direction axis
  let u = u0 .+. (v .-. v0) .*. du ./. dv
  if
    | dv > 0 -> Interval uLow (min uHigh u)
    | dv < 0 -> Interval (max uLow u) uHigh
    | otherwise -> Interval uLow uHigh
