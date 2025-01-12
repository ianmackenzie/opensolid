module OpenSolid.Surface.Function.Internal
  ( solveForU
  , solveForV
  , curveBounds
  , curveRangeBounds
  )
where

import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Solve1d qualified as Solve1d
import {-# SOURCE #-} OpenSolid.Surface.Function (Function)
import {-# SOURCE #-} OpenSolid.Surface.Function qualified as Function

solveForU ::
  Tolerance units =>
  Function units ->
  Function units ->
  Range Unitless ->
  Float ->
  Float
solveForU f fu uBounds vValue = do
  let uvPoint uValue = Point2d.xy uValue vValue
  let fValue uValue = Function.evaluate f (uvPoint uValue)
  let fuValue uValue = Function.evaluate fu (uvPoint uValue)
  case Solve1d.monotonic fValue fuValue uBounds of
    Solve1d.Exact uValue -> uValue
    Solve1d.Closest uValue -> uValue

solveForV ::
  Tolerance units =>
  Function units ->
  Function units ->
  Float ->
  Range Unitless ->
  Float
solveForV f fv uValue vBounds = do
  let uvPoint vValue = Point2d.xy uValue vValue
  let fValue vValue = Function.evaluate f (uvPoint vValue)
  let fvValue vValue = Function.evaluate fv (uvPoint vValue)
  case Solve1d.monotonic fValue fvValue vBounds of
    Solve1d.Exact vValue -> vValue
    Solve1d.Closest vValue -> vValue

curveBounds :: Float -> Float -> Qty units -> Qty units -> Range units -> Range units
curveBounds x1 x2 y1 y2 (Range mLow mHigh)
  | mLow >= Qty.zero || mHigh <= Qty.zero = Range.from y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 - x1
      let dY = y2 - y1
      let dXValley = Qty.clampTo (Range.from 0.0 dX) ((mHigh * dX - dY) / (mHigh - mLow))
      let dXPeak = Qty.clampTo (Range.from 0.0 dX) ((dY - mLow * dX) / (mHigh - mLow))
      let yValley = if Qty.isInfinite mLow then -Qty.infinity else y1 + mLow * dXValley
      let yPeak = if Qty.isInfinite mHigh then Qty.infinity else y1 + mHigh * dXPeak
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
      let dXValley = Qty.clampTo (Range.from 0.0 dX) ((mHigh * dX - dYLow) / (mHigh - mLow))
      let dXPeak = Qty.clampTo (Range.from 0.0 dX) ((dYHigh - mLow * dX) / (mHigh - mLow))
      let yValley = if Qty.isInfinite mLow then -Qty.infinity else low1 + mLow * dXValley
      let yPeak = if Qty.isInfinite mHigh then Qty.infinity else high1 + mHigh * dXPeak
      Range.from yValley yPeak
