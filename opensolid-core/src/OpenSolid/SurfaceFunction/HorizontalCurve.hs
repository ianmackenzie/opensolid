module OpenSolid.SurfaceFunction.HorizontalCurve
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
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.ImplicitCurveBounds (ImplicitCurveBounds)
import OpenSolid.SurfaceFunction.ImplicitCurveBounds qualified as ImplicitCurveBounds
import OpenSolid.SurfaceFunction.Internal qualified as Internal
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data MonotonicSpace

data Monotonicity
  = Monotonic
  | MonotonicIn (Frame2d UvSpace Unitless MonotonicSpace)
  | NotMonotonic
  deriving (Eq, Show)

implicitCurveBounds :: NonEmpty UvBounds -> ImplicitCurveBounds
implicitCurveBounds boxes =
  ImplicitCurveBounds.build (NonEmpty.map (\(Bounds2d u v) -> (u, v)) boxes)

new ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2d UvSpace Unitless
new derivatives dvdu uStart uEnd boxes =
  horizontalCurve derivatives dvdu uStart uEnd boxes NotMonotonic []

monotonic ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2d UvSpace Unitless
monotonic derivatives dvdu uStart uEnd boxes =
  horizontalCurve derivatives dvdu uStart uEnd boxes Monotonic []

bounded ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2d UvSpace Unitless defines ->
  List (Axis2d UvSpace Unitless) ->
  Curve2d UvSpace Unitless
bounded derivatives dvdu uStart uEnd boxes monotonicFrame boundingAxes = do
  let monotonicity = MonotonicIn (Frame2d.coerce monotonicFrame)
  horizontalCurve derivatives dvdu uStart uEnd boxes monotonicity boundingAxes

horizontalCurve ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Monotonicity ->
  List (Axis2d UvSpace Unitless) ->
  Curve2d UvSpace Unitless
horizontalCurve f dvdu uStart uEnd boxes monotonicity boundingAxes = do
  let bounds = implicitCurveBounds boxes
  let clampedVBounds uValue =
        List.foldl (clamp uValue) (ImplicitCurveBounds.evaluate bounds uValue) boundingAxes
  let solveForV =
        case (f.compiled, f.dv.compiled) of
          (CompiledFunction.Concrete fExpr, CompiledFunction.Concrete fvExpr) ->
            \uValue -> Expression.solveMonotonicSurfaceV fExpr fvExpr uValue (clampedVBounds uValue)
          _ -> \uValue -> Internal.solveForV f f.dv uValue (clampedVBounds uValue)
  let evaluate tValue = do
        let uValue = Number.interpolateFrom uStart uEnd tValue
        let vValue = solveForV uValue
        Point2d uValue vValue
  let evaluateBounds tBounds = do
        let Bounds t1 t2 = tBounds
        let u1 = Number.interpolateFrom uStart uEnd t1
        let u2 = Number.interpolateFrom uStart uEnd t2
        let v1 = solveForV u1
        let v2 = solveForV u2
        case monotonicity of
          Monotonic -> Bounds2d (Bounds u1 u2) (Bounds v1 v2)
          MonotonicIn frame -> do
            let p1 = Point2d u1 v1
            let p2 = Point2d u2 v2
            Bounds2d.hull2 (Point2d.relativeTo frame p1) (Point2d.relativeTo frame p2)
              & Bounds2d.placeIn frame
          NotMonotonic -> do
            let vBounds = ImplicitCurveBounds.evaluateBounds bounds (Bounds u1 u2)
            let slopeBounds = SurfaceFunction.evaluateBounds dvdu (Bounds2d (Bounds u1 u2) vBounds)
            let segmentVBounds = Internal.curveBoundsAt u1 u2 v1 v2 slopeBounds
            Bounds2d (Bounds u1 u2) segmentVBounds
  let derivative self = do
        let deltaU = uEnd .-. uStart
        let dudt = Curve.constant deltaU
        let dvdt = dudt .*. dvdu `compose` self
        VectorCurve2d.xy dudt dvdt
  Curve2d.recursive (CompiledFunction.abstract evaluate evaluateBounds) derivative

clamp :: Number -> Bounds Unitless -> Axis2d UvSpace Unitless -> Bounds Unitless
clamp u (Bounds vLow vHigh) axis = do
  let Point2d u0 v0 = Axis2d.originPoint axis
  let Direction2d du dv = Axis2d.direction axis
  let v = v0 .+. (u .-. u0) .*. dv ./. du
  if
    | du > 0 -> Bounds (max vLow v) vHigh
    | du < 0 -> Bounds vLow (min vHigh v)
    | otherwise -> Bounds vLow vHigh
