module OpenSolid.Surface1d.Function.HorizontalCurve
  ( HorizontalCurve
  , MonotonicSpace
  , new
  , monotonic
  , bounded
  )
where

import OpenSolid.Arithmetic.Unboxed
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.Point2d (Point2d (Point2d#))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import {-# SOURCE #-} OpenSolid.Surface1d.Function (Function)
import {-# SOURCE #-} OpenSolid.Surface1d.Function qualified as Function
import OpenSolid.Surface1d.Function.Internal qualified as Internal
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvCoordinates)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Uv.Derivatives (Derivatives)
import OpenSolid.Uv.Derivatives qualified as Derivatives
import OpenSolid.Vector2d (Vector2d (Vector2d#))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data HorizontalCurve units = HorizontalCurve
  { f :: Function units
  , fu :: Function units
  , fv :: Function units
  , dvdu :: Function Unitless
  , uStart :: Float
  , uEnd :: Float
  , vBounds :: Range Unitless
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

new ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Float ->
  Float ->
  Range Unitless ->
  Curve2d UvCoordinates
new derivatives dvdu uStart uEnd vBounds = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { f
      , fu
      , fv
      , dvdu
      , uStart
      , uEnd
      , vBounds
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
  Range Unitless ->
  Curve2d UvCoordinates
monotonic derivatives dvdu uStart uEnd vBounds = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { f
      , fu
      , fv
      , dvdu
      , uStart
      , uEnd
      , vBounds
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
  Range Unitless ->
  Frame2d UvCoordinates defines ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
bounded derivatives dvdu uStart uEnd vBounds monotonicFrame boundingAxes = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { f
      , fu
      , fv
      , dvdu
      , uStart
      , uEnd
      , vBounds
      , monotonicity = MonotonicIn (Frame2d.coerce monotonicFrame)
      , boundingAxes
      , tolerance = ?tolerance
      }

instance Curve2d.Interface (HorizontalCurve units) UvCoordinates where
  evaluateImpl curve tValue = do
    let HorizontalCurve{uStart, uEnd} = curve
    let uValue = Float.interpolateFrom uStart uEnd tValue
    let vValue = solveForV curve uValue
    Point2d.xy uValue vValue

  evaluateBoundsImpl curve tRange = do
    let HorizontalCurve{dvdu, uStart, uEnd, vBounds, monotonicity} = curve
    let Range t1 t2 = tRange
    let u1 = Float.interpolateFrom uStart uEnd t1
    let u2 = Float.interpolateFrom uStart uEnd t2
    let v1 = solveForV curve u1
    let v2 = solveForV curve u2
    case monotonicity of
      Monotonic -> Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      MonotonicIn frame -> do
        let p1 = Point2d.xy u1 v1
        let p2 = Point2d.xy u2 v2
        Bounds2d.hull2 (Point2d.relativeTo frame p1) (Point2d.relativeTo frame p2)
          |> Bounds2d.placeIn frame
      NotMonotonic -> do
        let slopeBounds = Function.evaluateBounds dvdu (Bounds2d.xy (Range.from u1 u2) vBounds)
        let segmentVBounds = Internal.curveBounds u1 u2 v1 v2 slopeBounds
        Bounds2d.xy (Range.from u1 u2) segmentVBounds

  derivativeImpl curve@(HorizontalCurve{dvdu, uStart, uEnd}) = do
    let deltaU = uEnd - uStart
    let dudt = Curve1d.constant deltaU
    let dvdt = dudt * dvdu . Curve2d.new curve
    VectorCurve2d.xy dudt dvdt

  reverseImpl
    HorizontalCurve
      { f
      , fu
      , fv
      , dvdu
      , uStart
      , uEnd
      , vBounds
      , monotonicity
      , boundingAxes
      , tolerance
      } =
      HorizontalCurve
        { f
        , fu
        , fv
        , dvdu
        , uStart = uEnd
        , uEnd = uStart
        , vBounds
        , monotonicity
        , boundingAxes
        , tolerance
        }

  transformByImpl transform curve =
    Curve2d.new (Curve2d.TransformBy transform curve)

solveForV :: HorizontalCurve units -> Float -> Float
solveForV (HorizontalCurve{f, fv, vBounds, boundingAxes, tolerance}) uValue = do
  let clampedBounds = List.foldl (clamp uValue) vBounds boundingAxes
  Tolerance.using tolerance (Internal.solveForV f fv uValue clampedBounds)

clamp :: Float -> Range Unitless -> Axis2d UvCoordinates -> Range Unitless
clamp (Qty# u#) (Range vLow vHigh) axis = do
  let !(Point2d# u0# v0#) = Axis2d.originPoint axis
  let !(Vector2d# du# dv#) = Direction2d.unwrap (Axis2d.direction axis)
  let v = Qty# (v0# +# (u# -# u0#) *# dv# /# du#)
  if
    | du# ># 0.0## -> Range (Qty.max vLow v) vHigh
    | du# <# 0.0## -> Range vLow (Qty.min vHigh v)
    | otherwise -> Range vLow vHigh
