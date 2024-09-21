module Surface1d.Function.Internal
  ( solveForU
  , solveForV
  , curveBounds
  , curveRangeBounds
  )
where

import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Solve1d qualified
import {-# SOURCE #-} Surface1d.Function (Function)
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Uv (Parameter (U, V))
import Uv.Derivatives (Derivatives)
import Uv.Derivatives qualified as Derivatives

solveForU ::
  Tolerance units =>
  Derivatives (Function units) ->
  Range Unitless ->
  Float ->
  Float
solveForU derivatives uBounds vValue = do
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  let uvPoint uValue = Point2d.xy uValue vValue
  let fValue uValue = Function.evaluate f (uvPoint uValue)
  let fuValue uValue = Function.evaluate fu (uvPoint uValue)
  Solve1d.monotonic fValue fuValue uBounds

solveForV ::
  Tolerance units =>
  Derivatives (Function units) ->
  Float ->
  Range Unitless ->
  Float
solveForV derivatives uValue vBounds = do
  let f = Derivatives.get derivatives
  let fv = Derivatives.get (derivatives >> V)
  let uvPoint vValue = Point2d.xy uValue vValue
  let fValue vValue = Function.evaluate f (uvPoint vValue)
  let fvValue vValue = Function.evaluate fv (uvPoint vValue)
  Solve1d.monotonic fValue fvValue vBounds

curveBounds :: Float -> Float -> Qty units -> Qty units -> Range units -> Range units
curveBounds x1 x2 y1 y2 (Range mLow mHigh)
  | mLow >= Qty.zero || mHigh <= Qty.zero = Range.from y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 - x1
      let dY = y2 - y1
      let dXValley = Qty.clamp 0.0 dX ((mHigh * dX - dY) / (mHigh - mLow))
      let dXPeak = Qty.clamp 0.0 dX ((dY - mLow * dX) / (mHigh - mLow))
      let yValley = y1 + mLow * dXValley
      let yPeak = y1 + mHigh * dXPeak
      Range.from yValley yPeak

curveRangeBounds :: Float -> Float -> Range units -> Range units -> Range units -> Range units
curveRangeBounds x1 x2 y1 y2 (Range mLow mHigh)
  | mLow >= Qty.zero || mHigh <= Qty.zero = Range.aggregate2 y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 - x1
      let Range low1 high1 = y1
      let Range low2 high2 = y2
      let dYLow = low2 - low1
      let dYHigh = high2 - high1
      let dXValley = Qty.clamp 0.0 dX ((mHigh * dX - dYLow) / (mHigh - mLow))
      let dXPeak = Qty.clamp 0.0 dX ((dYHigh - mLow * dX) / (mHigh - mLow))
      let yValley = low1 + mLow * dXValley
      let yPeak = high1 + mHigh * dXPeak
      Range.from yValley yPeak