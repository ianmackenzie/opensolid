module Surface1d.Function.HorizontalCurve
  ( HorizontalCurve
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

data HorizontalCurve units = HorizontalCurve
  { derivatives :: Derivatives (Function units)
  , dvdu :: Function Unitless
  , uStart :: Float
  , uEnd :: Float
  , vBounds :: Range Unitless
  , isMonotonic :: Bool
  , boundingAxes :: List (Axis2d Uv.Coordinates)
  , tolerance :: Qty units
  }
  deriving (Show)

new ::
  Tolerance units =>
  Derivatives (Function units) ->
  Float ->
  Float ->
  Range Unitless ->
  Curve2d Uv.Coordinates
new derivatives uStart uEnd vBounds = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { derivatives
      , dvdu = -fu / fv
      , uStart
      , uEnd
      , vBounds
      , isMonotonic = False
      , boundingAxes = []
      , tolerance = ?tolerance
      }

monotonic ::
  Tolerance units =>
  Derivatives (Function units) ->
  Float ->
  Float ->
  Range Unitless ->
  Curve2d Uv.Coordinates
monotonic derivatives uStart uEnd vBounds = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { derivatives
      , dvdu = -fu / fv
      , uStart
      , uEnd
      , vBounds
      , isMonotonic = True
      , boundingAxes = []
      , tolerance = ?tolerance
      }

bounded ::
  Tolerance units =>
  Derivatives (Function units) ->
  Float ->
  Float ->
  Range Unitless ->
  List (Axis2d Uv.Coordinates) ->
  Curve2d Uv.Coordinates
bounded derivatives uStart uEnd vBounds boundingAxes = do
  let fu = Derivatives.get (derivatives >> U)
  let fv = Derivatives.get (derivatives >> V)
  Curve2d.new $
    HorizontalCurve
      { derivatives
      , dvdu = -fu / fv
      , uStart
      , uEnd
      , vBounds
      , isMonotonic = False
      , boundingAxes
      , tolerance = ?tolerance
      }

instance Curve2d.Interface (HorizontalCurve units) Uv.Coordinates where
  startPointImpl curve = Curve2d.pointOnImpl curve 0.0
  endPointImpl curve = Curve2d.pointOnImpl curve 1.0

  pointOnImpl curve tValue = do
    let HorizontalCurve{uStart, uEnd} = curve
    let uValue = Float.interpolateFrom uStart uEnd tValue
    let vValue = solveForV curve uValue
    Point2d.xy uValue vValue

  segmentBoundsImpl curve tRange = do
    let HorizontalCurve{dvdu, uStart, uEnd, vBounds, isMonotonic} = curve
    let Range t1 t2 = tRange
    let u1 = Float.interpolateFrom uStart uEnd t1
    let u2 = Float.interpolateFrom uStart uEnd t2
    let v1 = solveForV curve u1
    let v2 = solveForV curve u2
    if isMonotonic
      then Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      else do
        let slopeBounds = Function.bounds dvdu (Bounds2d.xy (Range.from u1 u2) vBounds)
        let segmentVBounds = Internal.curveBounds u1 u2 v1 v2 slopeBounds
        Bounds2d.xy (Range.from u1 u2) segmentVBounds

  derivativeImpl curve@(HorizontalCurve{dvdu, uStart, uEnd}) = do
    let deltaU = uEnd - uStart
    let dudt = Curve1d.constant deltaU
    let dvdt = dudt * dvdu . Curve2d.new curve
    VectorCurve2d.xy dudt dvdt

  reverseImpl
    HorizontalCurve
      { derivatives
      , dvdu
      , uStart
      , uEnd
      , vBounds
      , isMonotonic
      , boundingAxes
      , tolerance
      } =
      HorizontalCurve
        { derivatives
        , dvdu
        , uStart = uEnd
        , uEnd = uStart
        , vBounds
        , isMonotonic
        , boundingAxes
        , tolerance
        }

  boundsImpl curve = Curve2d.segmentBoundsImpl curve Range.unit

  transformByImpl transform curve =
    Curve2d.new (Curve2d.TransformBy transform curve)

solveForV :: HorizontalCurve units -> Float -> Float
solveForV (HorizontalCurve{derivatives, vBounds, boundingAxes, tolerance}) uValue = do
  let clampedBounds = List.foldl (clamp uValue) vBounds boundingAxes
  Tolerance.using tolerance (Internal.solveForV derivatives uValue clampedBounds)

clamp :: Float -> Range Unitless -> Axis2d Uv.Coordinates -> Range Unitless
clamp u (Range vLow vHigh) axis = do
  let Point2d u0 v0 = Axis2d.originPoint axis
  let Vector2d du dv = Direction2d.unwrap (Axis2d.direction axis)
  let v = v0 + (u - u0) * dv / du
  if
    | du > 0.0 -> Range (Qty.max vLow v) vHigh
    | du < 0.0 -> Range vLow (Qty.min vHigh v)
    | otherwise -> Range vLow vHigh
