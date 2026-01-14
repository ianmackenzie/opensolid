module OpenSolid.SurfaceFunction.Internal
  ( solveForU
  , solveForV
  , curveBoundsAt
  , curveBoundsOver
  )
where

import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Solve1D qualified as Solve1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.UvPoint (pattern UvPoint)

solveForU ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction units ->
  Interval Unitless ->
  Number ->
  Number
solveForU f fu uBounds vValue = do
  let uvPoint uValue = UvPoint uValue vValue
  let fValue uValue = SurfaceFunction.evaluate f (uvPoint uValue)
  let fuValue uValue = SurfaceFunction.evaluate fu (uvPoint uValue)
  case Solve1D.monotonic fValue fuValue uBounds of
    Solve1D.Exact uValue -> uValue
    Solve1D.Closest uValue -> uValue

solveForV ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction units ->
  Number ->
  Interval Unitless ->
  Number
solveForV f fv uValue vBounds = do
  let uvPoint vValue = UvPoint uValue vValue
  let fValue vValue = SurfaceFunction.evaluate f (uvPoint vValue)
  let fvValue vValue = SurfaceFunction.evaluate fv (uvPoint vValue)
  case Solve1D.monotonic fValue fvValue vBounds of
    Solve1D.Exact vValue -> vValue
    Solve1D.Closest vValue -> vValue

curveBoundsAt :: Number -> Number -> Quantity units -> Quantity units -> Interval units -> Interval units
curveBoundsAt x1 x2 y1 y2 (Interval mLow mHigh)
  | mLow >= Quantity.zero || mHigh <= Quantity.zero = Interval y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 .-. x1
      let dY = y2 .-. y1
      let dXValley = Quantity.clampTo (Interval 0 dX) ((mHigh .*. dX .-. dY) ./. (mHigh .-. mLow))
      let dXPeak = Quantity.clampTo (Interval 0 dX) ((dY .-. mLow .*. dX) ./. (mHigh .-. mLow))
      let yValley =
            if Quantity.isInfinite mLow
              then negative Quantity.infinity
              else y1 .+. mLow .*. dXValley
      let yPeak =
            if Quantity.isInfinite mHigh
              then Quantity.infinity
              else y1 .+. mHigh .*. dXPeak
      Interval yValley yPeak

curveBoundsOver :: Number -> Number -> Interval units -> Interval units -> Interval units -> Interval units
curveBoundsOver x1 x2 y1 y2 (Interval mLow mHigh)
  | mLow >= Quantity.zero || mHigh <= Quantity.zero = Interval.aggregate2 y1 y2 -- Monotonic case
  | otherwise = do
      let dX = x2 .-. x1
      let Interval low1 high1 = y1
      let Interval low2 high2 = y2
      let dYLow = low2 .-. low1
      let dYHigh = high2 .-. high1
      let dXValley = Quantity.clampTo (Interval 0 dX) ((mHigh .*. dX .-. dYLow) ./. (mHigh .-. mLow))
      let dXPeak = Quantity.clampTo (Interval 0 dX) ((dYHigh .-. mLow .*. dX) ./. (mHigh .-. mLow))
      let yValley =
            if Quantity.isInfinite mLow
              then negative Quantity.infinity
              else low1 .+. mLow .*. dXValley
      let yPeak =
            if Quantity.isInfinite mHigh
              then Quantity.infinity
              else high1 .+. mHigh .*. dXPeak
      Interval yValley yPeak
