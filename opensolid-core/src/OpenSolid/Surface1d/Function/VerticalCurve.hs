module OpenSolid.Surface1d.Function.VerticalCurve
  ( VerticalCurve
  , new
  , monotonic
  , bounded
  )
where

import OpenSolid.Arithmetic.Unboxed
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d#))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import {-# SOURCE #-} OpenSolid.Surface1d.Function (Function)
import {-# SOURCE #-} OpenSolid.Surface1d.Function qualified as Function
import OpenSolid.Surface1d.Function.ImplicitCurveBounds (ImplicitCurveBounds)
import OpenSolid.Surface1d.Function.ImplicitCurveBounds qualified as ImplicitCurveBounds
import OpenSolid.Surface1d.Function.Internal qualified as Internal
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Uv.Derivatives (Derivatives)
import OpenSolid.Uv.Derivatives qualified as Derivatives
import OpenSolid.Vector2d (Vector2d (Vector2d#))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data VerticalCurve units = VerticalCurve
  { f :: Function units
  , fu :: Function units
  , fv :: Function units
  , dudv :: Function Unitless
  , bounds :: ImplicitCurveBounds
  , vStart :: Float
  , vEnd :: Float
  , monotonicity :: Monotonicity
  , boundingAxes :: List (Axis2d UvCoordinates)
  , tolerance :: Qty units
  }
  deriving (Show)

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
  Derivatives (Function units) ->
  Function Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
new derivatives dudv vStart vEnd boxes = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { f
      , fu
      , fv
      , dudv
      , bounds = implicitCurveBounds boxes
      , vStart
      , vEnd
      , monotonicity = NotMonotonic
      , boundingAxes = []
      , tolerance = ?tolerance
      }

monotonic ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
monotonic derivatives dudv vStart vEnd boxes = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { f
      , fu
      , fv
      , dudv
      , bounds = implicitCurveBounds boxes
      , vStart
      , vEnd
      , monotonicity = Monotonic
      , boundingAxes = []
      , tolerance = ?tolerance
      }

bounded ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Frame2d UvCoordinates defines ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
bounded derivatives dudv vStart vEnd boxes monotonicFrame boundingAxes = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { f
      , fu
      , fv
      , dudv
      , bounds = implicitCurveBounds boxes
      , vStart
      , vEnd
      , monotonicity = MonotonicIn (Frame2d.coerce monotonicFrame)
      , boundingAxes
      , tolerance = ?tolerance
      }

instance Curve2d.Interface (VerticalCurve units) UvCoordinates where
  evaluateImpl curve tValue = do
    let VerticalCurve{vStart, vEnd} = curve
    let vValue = Float.interpolateFrom vStart vEnd tValue
    let uValue = solveForU curve vValue
    Point2d.xy uValue vValue

  evaluateBoundsImpl curve tRange = do
    let VerticalCurve{dudv, bounds, vStart, vEnd, monotonicity} = curve
    let Range t1 t2 = tRange
    let v1 = Float.interpolateFrom vStart vEnd t1
    let v2 = Float.interpolateFrom vStart vEnd t2
    let u1 = solveForU curve v1
    let u2 = solveForU curve v2
    case monotonicity of
      Monotonic -> Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      MonotonicIn frame -> do
        let p1 = Point2d.xy u1 v1
        let p2 = Point2d.xy u2 v2
        Bounds2d.hull2 (Point2d.relativeTo frame p1) (Point2d.relativeTo frame p2)
          |> Bounds2d.placeIn frame
      NotMonotonic -> do
        let uRange = ImplicitCurveBounds.evaluateBounds bounds (Range.from v1 v2)
        let slopeBounds = Function.evaluateBounds dudv (Bounds2d.xy uRange (Range.from v1 v2))
        let segmentUBounds = Internal.curveBounds v1 v2 u1 u2 slopeBounds
        Bounds2d.xy segmentUBounds (Range.from v1 v2)

  derivativeImpl curve@(VerticalCurve{dudv, vStart, vEnd}) = do
    let deltaV = vEnd - vStart
    let dvdt = Curve1d.constant deltaV
    let dudt = dvdt * dudv . Curve2d.new curve
    VectorCurve2d.xy dudt dvdt

  reverseImpl
    VerticalCurve
      { f
      , fu
      , fv
      , dudv
      , bounds
      , vStart
      , vEnd
      , monotonicity
      , boundingAxes
      , tolerance
      } =
      VerticalCurve
        { f
        , fu
        , fv
        , dudv
        , bounds
        , vStart = vEnd
        , vEnd = vStart
        , monotonicity
        , boundingAxes
        , tolerance
        }

  transformByImpl transform curve =
    Curve2d.Transformed transform (Curve2d.new curve)

solveForU :: VerticalCurve units -> Float -> Float
solveForU (VerticalCurve{f, fu, bounds, boundingAxes, tolerance}) vValue = do
  let uRange = ImplicitCurveBounds.evaluate bounds vValue
  let clampedBounds = List.foldl (clamp vValue) uRange boundingAxes
  Tolerance.using tolerance (Internal.solveForU f fu clampedBounds vValue)

clamp :: Float -> Range Unitless -> Axis2d UvCoordinates -> Range Unitless
clamp (Qty# v#) (Range uLow uHigh) axis = do
  let !(Point2d# u0# v0#) = Axis2d.originPoint axis
  let !(Vector2d# du# dv#) = Direction2d.unwrap (Axis2d.direction axis)
  let u = Qty# (u0# +# (v# -# v0#) *# du# /# dv#)
  if
    | dv# ># 0.0## -> Range uLow (Qty.min uHigh u)
    | dv# <# 0.0## -> Range (Qty.max uLow u) uHigh
    | otherwise -> Range uLow uHigh
