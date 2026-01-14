module OpenSolid.SurfaceFunction1D.HorizontalCurve
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
  ImplicitCurveBounds.build (NonEmpty.map (\(Bounds2D u v) -> (u, v)) boxes)

new ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
new derivatives dvdu uStart uEnd boxes =
  horizontalCurve derivatives dvdu uStart uEnd boxes NotMonotonic []

monotonic ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
monotonic derivatives dvdu uStart uEnd boxes =
  horizontalCurve derivatives dvdu uStart uEnd boxes Monotonic []

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
bounded derivatives dvdu uStart uEnd boxes monotonicFrame boundingAxes = do
  let monotonicity = MonotonicIn (Frame2D.coerce monotonicFrame)
  horizontalCurve derivatives dvdu uStart uEnd boxes monotonicity boundingAxes

horizontalCurve ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Monotonicity ->
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
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
        UvPoint uValue vValue
  let evaluateBounds tBounds = do
        let Interval t1 t2 = tBounds
        let u1 = Number.interpolateFrom uStart uEnd t1
        let u2 = Number.interpolateFrom uStart uEnd t2
        let v1 = solveForV u1
        let v2 = solveForV u2
        case monotonicity of
          Monotonic -> Bounds2D (Interval u1 u2) (Interval v1 v2)
          MonotonicIn frame -> do
            let p1 = UvPoint u1 v1
            let p2 = UvPoint u2 v2
            Bounds2D.hull2 (Point2D.relativeTo frame p1) (Point2D.relativeTo frame p2)
              & Bounds2D.placeIn frame
          NotMonotonic -> do
            let vBounds = ImplicitCurveBounds.evaluateBounds bounds (Interval u1 u2)
            let slopeBounds = SurfaceFunction1D.evaluateBounds dvdu (Bounds2D (Interval u1 u2) vBounds)
            let segmentVBounds = Internal.curveBoundsAt u1 u2 v1 v2 slopeBounds
            Bounds2D (Interval u1 u2) segmentVBounds
  let derivative self = do
        let deltaU = uEnd .-. uStart
        let dudt = Curve1D.constant deltaU
        let dvdt = dudt .*. dvdu `compose` self
        VectorCurve2D.xy dudt dvdt
  Curve2D.recursive (CompiledFunction.abstract evaluate evaluateBounds) derivative

clamp :: Number -> Interval Unitless -> Axis2D Unitless UvSpace -> Interval Unitless
clamp u (Interval vLow vHigh) axis = do
  let UvPoint u0 v0 = Axis2D.originPoint axis
  let Direction2D du dv = Axis2D.direction axis
  let v = v0 .+. (u .-. u0) .*. dv ./. du
  if
    | du > 0 -> Interval (max vLow v) vHigh
    | du < 0 -> Interval vLow (min vHigh v)
    | otherwise -> Interval vLow vHigh
