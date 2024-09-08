module Surface1d.Function.VerticalCurve
  ( VerticalCurve
  , new
  , monotonic
  , bounded
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import Bounds2d qualified
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import {-# SOURCE #-} Surface1d.Function (Function)
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Surface1d.Function.Internal qualified as Internal
import Tolerance
import Uv (Parameter (U, V))
import Uv qualified
import Uv.Derivatives (Derivatives)
import Uv.Derivatives qualified as Derivatives
import Vector2d (Vector2d (Vector2d))
import VectorCurve2d qualified

data VerticalCurve units = VerticalCurve
  { derivatives :: Derivatives (Function units)
  , dudv :: Function Unitless
  , uBounds :: Range Unitless
  , vStart :: Float
  , vEnd :: Float
  , monotonicity :: Monotonicity
  , boundingAxes :: List (Axis2d Uv.Coordinates)
  , tolerance :: Qty units
  }
  deriving (Show)

data MonotonicSpace

data Monotonicity
  = Monotonic
  | MonotonicIn (Frame2d Uv.Coordinates (Defines MonotonicSpace))
  | NotMonotonic
  deriving (Show)

new ::
  Tolerance units =>
  Derivatives (Function units) ->
  Range Unitless ->
  Float ->
  Float ->
  Curve2d Uv.Coordinates
new derivatives uBounds vStart vEnd = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { derivatives
      , dudv = -fv / fu
      , uBounds
      , vStart
      , vEnd
      , monotonicity = NotMonotonic
      , boundingAxes = []
      , tolerance = ?tolerance
      }

monotonic ::
  Tolerance units =>
  Derivatives (Function units) ->
  Range Unitless ->
  Float ->
  Float ->
  Curve2d Uv.Coordinates
monotonic derivatives uBounds vStart vEnd = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { derivatives
      , dudv = -fv / fu
      , uBounds
      , vStart
      , vEnd
      , monotonicity = Monotonic
      , boundingAxes = []
      , tolerance = ?tolerance
      }

bounded ::
  Tolerance units =>
  Derivatives (Function units) ->
  Range Unitless ->
  Float ->
  Float ->
  Frame2d Uv.Coordinates defines ->
  List (Axis2d Uv.Coordinates) ->
  Curve2d Uv.Coordinates
bounded derivatives uBounds vStart vEnd monotonicFrame boundingAxes = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    VerticalCurve
      { derivatives
      , dudv = -fv / fu
      , uBounds
      , vStart
      , vEnd
      , monotonicity = MonotonicIn (Frame2d.coerce monotonicFrame)
      , boundingAxes
      , tolerance = ?tolerance
      }

instance Curve2d.Interface (VerticalCurve units) Uv.Coordinates where
  startPointImpl curve = Curve2d.pointOnImpl curve 0.0
  endPointImpl curve = Curve2d.pointOnImpl curve 1.0

  pointOnImpl curve tValue = do
    let VerticalCurve{vStart, vEnd} = curve
    let vValue = Float.interpolateFrom vStart vEnd tValue
    let uValue = solveForU curve vValue
    Point2d.xy uValue vValue

  segmentBoundsImpl curve tRange = do
    let VerticalCurve{dudv, uBounds, vStart, vEnd, monotonicity} = curve
    let Range t1 t2 = tRange
    let v1 = Float.interpolateFrom vStart vEnd t1
    let v2 = Float.interpolateFrom vStart vEnd t2
    let u1 = solveForU curve v1
    let u2 = solveForU curve v2
    case monotonicity of
      Monotonic -> Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      MonotonicIn frame -> do
        let p1 = Point2d u1 v1
        let p2 = Point2d u2 v2
        Bounds2d.hull2 (Point2d.relativeTo frame p1) (Point2d.relativeTo frame p2)
          |> Bounds2d.placeIn frame
      NotMonotonic -> do
        let slopeBounds = Function.bounds dudv (Bounds2d.xy uBounds (Range.from v1 v2))
        let segmentUBounds = Internal.curveBounds v1 v2 u1 u2 slopeBounds
        Bounds2d.xy segmentUBounds (Range.from v1 v2)

  derivativeImpl curve@(VerticalCurve{dudv, vStart, vEnd}) = do
    let deltaV = vEnd - vStart
    let dvdt = Curve1d.constant deltaV
    let dudt = dvdt * dudv . Curve2d.new curve
    VectorCurve2d.xy dudt dvdt

  reverseImpl
    VerticalCurve
      { derivatives
      , dudv
      , uBounds
      , vStart
      , vEnd
      , monotonicity
      , boundingAxes
      , tolerance
      } =
      VerticalCurve
        { derivatives
        , dudv
        , uBounds
        , vStart = vEnd
        , vEnd = vStart
        , monotonicity
        , boundingAxes
        , tolerance
        }

  boundsImpl curve = Curve2d.segmentBoundsImpl curve Range.unit

  transformByImpl transform curve =
    Curve2d.new (Curve2d.TransformBy transform curve)

solveForU :: VerticalCurve units -> Float -> Float
solveForU (VerticalCurve{derivatives, uBounds, boundingAxes, tolerance}) vValue = do
  let clampedBounds = List.foldl (clamp vValue) uBounds boundingAxes
  Tolerance.using tolerance (Internal.solveForU derivatives clampedBounds vValue)

clamp :: Float -> Range Unitless -> Axis2d Uv.Coordinates -> Range Unitless
clamp v (Range uLow uHigh) axis = do
  let Point2d u0 v0 = Axis2d.originPoint axis
  let Vector2d du dv = Direction2d.unwrap (Axis2d.direction axis)
  let u = u0 + (v - v0) * du / dv
  if
    | dv > 0.0 -> Range uLow (Qty.min uHigh u)
    | dv < 0.0 -> Range (Qty.max uLow u) uHigh
    | otherwise -> Range uLow uHigh
