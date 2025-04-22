module OpenSolid.SurfaceFunction.Internal
  ( solveForU
  , solveForV
  , curveBoundsAt
  , curveBoundsOver
  )
where

import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Solve1d qualified as Solve1d
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction

solveForU ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction units ->
  Bounds Unitless ->
  Float ->
  Float
solveForU f fu uBounds vValue = do
  let uvPoint uValue = Point2d.xy uValue vValue
  let fValue uValue = SurfaceFunction.evaluate f (uvPoint uValue)
  let fuValue uValue = SurfaceFunction.evaluate fu (uvPoint uValue)
  case Solve1d.monotonic fValue fuValue uBounds of
    Solve1d.Exact uValue -> uValue
    Solve1d.Closest uValue -> uValue

solveForV ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction units ->
  Float ->
  Bounds Unitless ->
  Float
solveForV f fv uValue vBounds = do
  let uvPoint vValue = Point2d.xy uValue vValue
  let fValue vValue = SurfaceFunction.evaluate f (uvPoint vValue)
  let fvValue vValue = SurfaceFunction.evaluate fv (uvPoint vValue)
  case Solve1d.monotonic fValue fvValue vBounds of
    Solve1d.Exact vValue -> vValue
    Solve1d.Closest vValue -> vValue

curveBoundsAt :: Float -> Float -> Qty units -> Qty units -> Bounds units -> Bounds units
curveBoundsAt x1 x2 y1 y2 (Bounds mLow mHigh)
  | mLow >= Qty.zero || mHigh <= Qty.zero = Bounds y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 - x1
      let dY = y2 - y1
      let dXValley = Qty.clampTo (Bounds 0.0 dX) ((mHigh * dX - dY) / (mHigh - mLow))
      let dXPeak = Qty.clampTo (Bounds 0.0 dX) ((dY - mLow * dX) / (mHigh - mLow))
      let yValley = if Qty.isInfinite mLow then -Qty.infinity else y1 + mLow * dXValley
      let yPeak = if Qty.isInfinite mHigh then Qty.infinity else y1 + mHigh * dXPeak
      Bounds yValley yPeak

curveBoundsOver :: Float -> Float -> Bounds units -> Bounds units -> Bounds units -> Bounds units
curveBoundsOver x1 x2 y1 y2 (Bounds mLow mHigh)
  | mLow >= Qty.zero || mHigh <= Qty.zero = Bounds.aggregate2 y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 - x1
      let Bounds low1 high1 = y1
      let Bounds low2 high2 = y2
      let dYLow = low2 - low1
      let dYHigh = high2 - high1
      let dXValley = Qty.clampTo (Bounds 0.0 dX) ((mHigh * dX - dYLow) / (mHigh - mLow))
      let dXPeak = Qty.clampTo (Bounds 0.0 dX) ((dYHigh - mLow * dX) / (mHigh - mLow))
      let yValley = if Qty.isInfinite mLow then -Qty.infinity else low1 + mLow * dXValley
      let yPeak = if Qty.isInfinite mHigh then Qty.infinity else high1 + mHigh * dXPeak
      Bounds yValley yPeak
