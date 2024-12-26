module OpenSolid.VectorCurve3d.Direction (unsafe) where

import {-# SOURCE #-} DirectionCurve3d (DirectionCurve3d)
import {-# SOURCE #-} DirectionCurve3d qualified
import Maybe qualified
import OpenSolid.Prelude
import OpenSolid.Vector3d qualified as Vector3d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorCurve3d.DegenerateEndpoint (DegenerateEndpoint)
import OpenSolid.VectorCurve3d.DegenerateEndpoint qualified as DegenerateEndpoint
import Range (Range (Range))
import Range qualified
import VectorBounds3d qualified

data PiecewiseCurve space
  = PiecewiseCurve
      (Maybe (DegenerateEndpoint space))
      (VectorCurve3d (space @ Unitless))
      (Maybe (DegenerateEndpoint space))
  deriving (Show)

instance VectorCurve3d.Interface (PiecewiseCurve space) (space @ Unitless) where
  evaluateImpl (PiecewiseCurve Nothing inner Nothing) tValue =
    VectorCurve3d.evaluate inner tValue
  evaluateImpl (PiecewiseCurve (Just degenerateStart) inner Nothing) tValue
    | tValue >= DegenerateEndpoint.cutoff degenerateStart = VectorCurve3d.evaluate inner tValue
    | otherwise = DegenerateEndpoint.evaluate degenerateStart inner tValue
  evaluateImpl (PiecewiseCurve Nothing inner (Just degenerateEnd)) tValue
    | tValue <= DegenerateEndpoint.cutoff degenerateEnd = VectorCurve3d.evaluate inner tValue
    | otherwise = DegenerateEndpoint.evaluate degenerateEnd inner tValue
  evaluateImpl (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd)) tValue
    | tValue < DegenerateEndpoint.cutoff degenerateStart =
        DegenerateEndpoint.evaluate degenerateStart inner tValue
    | tValue > DegenerateEndpoint.cutoff degenerateEnd =
        DegenerateEndpoint.evaluate degenerateEnd inner tValue
    | otherwise = VectorCurve3d.evaluate inner tValue

  evaluateBoundsImpl (PiecewiseCurve Nothing inner Nothing) tRange =
    VectorCurve3d.evaluateBounds inner tRange
  evaluateBoundsImpl (PiecewiseCurve (Just degenerateStart) inner Nothing) tRange = do
    let (Range t1 t2) = tRange
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    if
      | t1 >= tStart -> VectorCurve3d.evaluateBounds inner tRange
      | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
      | otherwise ->
          VectorBounds3d.aggregate2
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve3d.evaluateBounds inner (Range.from tStart t2))
  evaluateBoundsImpl (PiecewiseCurve Nothing inner (Just degenerateEnd)) tRange = do
    let (Range t1 t2) = tRange
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t2 <= tEnd -> VectorCurve3d.evaluateBounds inner tRange
      | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
      | otherwise ->
          VectorBounds3d.aggregate2
            (VectorCurve3d.evaluateBounds inner (Range.from t1 tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))
  evaluateBoundsImpl (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd)) tRange = do
    let (Range t1 t2) = tRange
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t1 >= tStart && t2 <= tEnd -> VectorCurve3d.evaluateBounds inner tRange
      | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
      | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
      | t1 >= tStart ->
          VectorBounds3d.aggregate2
            (VectorCurve3d.evaluateBounds inner (Range.from t1 tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))
      | t2 <= tEnd ->
          VectorBounds3d.aggregate2
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve3d.evaluateBounds inner (Range.from tStart t2))
      | otherwise ->
          VectorBounds3d.aggregate3
            (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range.from t1 tStart))
            (VectorCurve3d.evaluateBounds inner (Range.from tStart tEnd))
            (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range.from tEnd t2))

  derivativeImpl (PiecewiseCurve start general end) =
    VectorCurve3d.new $
      PiecewiseCurve
        (Maybe.map DegenerateEndpoint.derivative start)
        (VectorCurve3d.derivative general)
        (Maybe.map DegenerateEndpoint.derivative end)

  transformByImpl transform (PiecewiseCurve start general end) =
    VectorCurve3d.new $
      PiecewiseCurve
        (Maybe.map (DegenerateEndpoint.transformBy transform) start)
        (VectorCurve3d.transformBy transform general)
        (Maybe.map (DegenerateEndpoint.transformBy transform) end)

unsafe ::
  Tolerance units =>
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units) ->
  DirectionCurve3d space
unsafe firstDerivative secondDerivative = do
  let degenerateStart = endpoint 0.0 firstDerivative secondDerivative
  let degenerateEnd = endpoint 1.0 firstDerivative secondDerivative
  let normalized = firstDerivative / VectorCurve3d.unsafeMagnitude firstDerivative
  DirectionCurve3d.unsafe $
    case (degenerateStart, degenerateEnd) of
      (Nothing, Nothing) -> normalized
      _ -> VectorCurve3d.new (PiecewiseCurve degenerateStart normalized degenerateEnd)

endpoint ::
  Tolerance units =>
  Float ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units) ->
  Maybe (DegenerateEndpoint space)
endpoint t0 firstDerivative secondDerivative
  | VectorCurve3d.evaluate firstDerivative t0 ~= Vector3d.zero =
      Just (DegenerateEndpoint.at t0 secondDerivative)
  | otherwise = Nothing
