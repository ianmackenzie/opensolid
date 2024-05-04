module VectorCurve2d.Direction (unsafe) where

import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} DirectionCurve2d qualified
import Maybe qualified
import OpenSolid
import Range (Range (Range))
import Range qualified
import Vector2d qualified
import VectorBounds2d qualified
import {-# SOURCE #-} VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified
import VectorCurve2d.DegenerateEndpoint (DegenerateEndpoint)
import VectorCurve2d.DegenerateEndpoint qualified as DegenerateEndpoint

data PiecewiseCurve space
  = PiecewiseCurve
      (Maybe (DegenerateEndpoint space))
      (VectorCurve2d (space @ Unitless))
      (Maybe (DegenerateEndpoint space))
  deriving (Show)

instance VectorCurve2d.Interface (PiecewiseCurve space) (space @ Unitless) where
  evaluateAtImpl t (PiecewiseCurve Nothing inner Nothing) = VectorCurve2d.evaluateAt t inner
  evaluateAtImpl t (PiecewiseCurve (Just degenerateStart) inner Nothing)
    | t >= DegenerateEndpoint.cutoff degenerateStart = VectorCurve2d.evaluateAt t inner
    | otherwise = DegenerateEndpoint.evaluateAt t degenerateStart inner
  evaluateAtImpl t (PiecewiseCurve Nothing inner (Just degenerateEnd))
    | t <= DegenerateEndpoint.cutoff degenerateEnd = VectorCurve2d.evaluateAt t inner
    | otherwise = DegenerateEndpoint.evaluateAt t degenerateEnd inner
  evaluateAtImpl t (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd))
    | t < DegenerateEndpoint.cutoff degenerateStart =
        DegenerateEndpoint.evaluateAt t degenerateStart inner
    | t > DegenerateEndpoint.cutoff degenerateEnd =
        DegenerateEndpoint.evaluateAt t degenerateEnd inner
    | otherwise = VectorCurve2d.evaluateAt t inner

  segmentBoundsImpl t (PiecewiseCurve Nothing inner Nothing) = VectorCurve2d.segmentBounds t inner
  segmentBoundsImpl t (PiecewiseCurve (Just degenerateStart) inner Nothing) = do
    let (Range t1 t2) = t
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    if
      | t1 >= tStart -> VectorCurve2d.segmentBounds t inner
      | t2 <= tStart -> DegenerateEndpoint.segmentBounds t degenerateStart inner
      | otherwise ->
          VectorBounds2d.aggregate2
            (DegenerateEndpoint.segmentBounds (Range.from t1 tStart) degenerateStart inner)
            (VectorCurve2d.segmentBounds (Range.from tStart t2) inner)
  segmentBoundsImpl t (PiecewiseCurve Nothing inner (Just degenerateEnd)) = do
    let (Range t1 t2) = t
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t2 <= tEnd -> VectorCurve2d.segmentBounds t inner
      | t1 >= tEnd -> DegenerateEndpoint.segmentBounds t degenerateEnd inner
      | otherwise ->
          VectorBounds2d.aggregate2
            (VectorCurve2d.segmentBounds (Range.from t1 tEnd) inner)
            (DegenerateEndpoint.segmentBounds (Range.from tEnd t2) degenerateEnd inner)
  segmentBoundsImpl t (PiecewiseCurve (Just degenerateStart) inner (Just degenerateEnd)) = do
    let (Range t1 t2) = t
    let tStart = DegenerateEndpoint.cutoff degenerateStart
    let tEnd = DegenerateEndpoint.cutoff degenerateEnd
    if
      | t1 >= tStart && t2 <= tEnd -> VectorCurve2d.segmentBounds t inner
      | t2 <= tStart -> DegenerateEndpoint.segmentBounds t degenerateStart inner
      | t1 >= tEnd -> DegenerateEndpoint.segmentBounds t degenerateEnd inner
      | t1 >= tStart ->
          VectorBounds2d.aggregate2
            (VectorCurve2d.segmentBounds (Range.from t1 tEnd) inner)
            (DegenerateEndpoint.segmentBounds (Range.from tEnd t2) degenerateEnd inner)
      | t2 <= tEnd ->
          VectorBounds2d.aggregate2
            (DegenerateEndpoint.segmentBounds (Range.from t1 tStart) degenerateStart inner)
            (VectorCurve2d.segmentBounds (Range.from tStart t2) inner)
      | otherwise ->
          VectorBounds2d.aggregate3
            (DegenerateEndpoint.segmentBounds (Range.from t1 tStart) degenerateStart inner)
            (VectorCurve2d.segmentBounds (Range.from tStart tEnd) inner)
            (DegenerateEndpoint.segmentBounds (Range.from tEnd t2) degenerateEnd inner)

  derivativeImpl (PiecewiseCurve start general end) =
    VectorCurve2d.wrap $
      PiecewiseCurve
        (Maybe.map DegenerateEndpoint.derivative start)
        (VectorCurve2d.derivative general)
        (Maybe.map DegenerateEndpoint.derivative end)

  transformByImpl transform (PiecewiseCurve start general end) =
    VectorCurve2d.wrap $
      PiecewiseCurve
        (Maybe.map (DegenerateEndpoint.transformBy transform) start)
        (VectorCurve2d.transformBy transform general)
        (Maybe.map (DegenerateEndpoint.transformBy transform) end)

unsafe ::
  Tolerance units =>
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  DirectionCurve2d space
unsafe firstDerivative secondDerivative =
  DirectionCurve2d.unsafe $
    VectorCurve2d.wrap $
      PiecewiseCurve
        (endpoint 0.0 firstDerivative secondDerivative)
        (firstDerivative / VectorCurve2d.unsafeMagnitude firstDerivative)
        (endpoint 1.0 firstDerivative secondDerivative)

endpoint ::
  Tolerance units =>
  Float ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  Maybe (DegenerateEndpoint space)
endpoint t0 firstDerivative secondDerivative
  | VectorCurve2d.evaluateAt t0 firstDerivative ~= Vector2d.zero =
      Just (DegenerateEndpoint.at t0 secondDerivative)
  | otherwise = Nothing
