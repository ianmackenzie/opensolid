module OpenSolid.VectorCurve2d.Direction (unsafe) where

import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} DirectionCurve2d qualified
import Maybe qualified
import OpenSolid.Prelude
import Range (Range (Range))
import Range qualified
import OpenSolid.Vector2d qualified as Vector2d
import VectorBounds2d qualified
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve2d.DegenerateEndpoint (DegenerateEndpoint)
import OpenSolid.VectorCurve2d.DegenerateEndpoint qualified as DegenerateEndpoint

data PiecewiseCurve space
  = PiecewiseCurve
      (Maybe (DegenerateEndpoint space))
      (VectorCurve2d (space @ Unitless))
      (Maybe (DegenerateEndpoint space))
  deriving (Show)

instance VectorCurve2d.Interface (PiecewiseCurve space) (space @ Unitless) where
  evaluateImpl (PiecewiseCurve Nothing inner Nothing) tValue =
    VectorCurve2d.evaluate inner tValue
  evaluateImpl (PiecewiseCurve (Just degenerateStart) inner Nothing) tValue
    | tValue >= DegenerateEndpoint.cutoff degenerateStart = VectorCurve2d.evaluate inner tValue
    | otherwise = DegenerateEndpoint.evaluate degenerateStart inner tValue
  evaluateImpl (PiecewiseCurve Nothing inner (Just degenerateEnd)) tValue
    | tValue <= DegenerateEndpoint.cutoff degenerateEnd = VectorCurve2d.evaluate inner tValue
    | otherwise = DegenerateEndpoint.evaluate degenerateEnd inner tValue
  evaluateImpl (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd)) tValue
    | tValue < DegenerateEndpoint.cutoff degenerateStart =
        DegenerateEndpoint.evaluate degenerateStart inner tValue
    | tValue > DegenerateEndpoint.cutoff degenerateEnd =
        DegenerateEndpoint.evaluate degenerateEnd inner tValue
    | otherwise = VectorCurve2d.evaluate inner tValue

  evaluateBoundsImpl (PiecewiseCurve Nothing inner Nothing) tRange =
    VectorCurve2d.evaluateBounds inner tRange
  evaluateBoundsImpl (PiecewiseCurve (Just degenerateStart) inner Nothing) tRange = do
    let (Range t1 t2) = tRange
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    if
      | t1 >= tStart -> VectorCurve2d.evaluateBounds inner tRange
      | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
      | otherwise ->
          VectorBounds2d.aggregate2
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve2d.evaluateBounds inner (Range.from tStart t2))
  evaluateBoundsImpl (PiecewiseCurve Nothing inner (Just degenerateEnd)) tRange = do
    let (Range t1 t2) = tRange
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t2 <= tEnd -> VectorCurve2d.evaluateBounds inner tRange
      | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
      | otherwise ->
          VectorBounds2d.aggregate2
            (VectorCurve2d.evaluateBounds inner (Range.from t1 tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))
  evaluateBoundsImpl (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd)) tRange = do
    let (Range t1 t2) = tRange
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t1 >= tStart && t2 <= tEnd -> VectorCurve2d.evaluateBounds inner tRange
      | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
      | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
      | t1 >= tStart ->
          VectorBounds2d.aggregate2
            (VectorCurve2d.evaluateBounds inner (Range.from t1 tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))
      | t2 <= tEnd ->
          VectorBounds2d.aggregate2
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve2d.evaluateBounds inner (Range.from tStart t2))
      | otherwise ->
          VectorBounds2d.aggregate3
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve2d.evaluateBounds inner (Range.from tStart tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))

  derivativeImpl (PiecewiseCurve start general end) =
    VectorCurve2d.new $
      PiecewiseCurve
        (Maybe.map DegenerateEndpoint.derivative start)
        (VectorCurve2d.derivative general)
        (Maybe.map DegenerateEndpoint.derivative end)

  transformByImpl transform (PiecewiseCurve start general end) =
    VectorCurve2d.new $
      PiecewiseCurve
        (Maybe.map (DegenerateEndpoint.transformBy transform) start)
        (VectorCurve2d.transformBy transform general)
        (Maybe.map (DegenerateEndpoint.transformBy transform) end)

unsafe ::
  Tolerance units =>
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  DirectionCurve2d space
unsafe firstDerivative secondDerivative = do
  let degenerateStart = endpoint 0.0 firstDerivative secondDerivative
  let degenerateEnd = endpoint 1.0 firstDerivative secondDerivative
  let normalized = firstDerivative / VectorCurve2d.unsafeMagnitude firstDerivative
  DirectionCurve2d.unsafe $
    case (degenerateStart, degenerateEnd) of
      (Nothing, Nothing) -> normalized
      _ -> VectorCurve2d.new (PiecewiseCurve degenerateStart normalized degenerateEnd)

endpoint ::
  Tolerance units =>
  Float ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  Maybe (DegenerateEndpoint space)
endpoint t0 firstDerivative secondDerivative
  | VectorCurve2d.evaluate firstDerivative t0 ~= Vector2d.zero =
      Just (DegenerateEndpoint.at t0 secondDerivative)
  | otherwise = Nothing
