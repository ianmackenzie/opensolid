module OpenSolid.VectorCurve2d.Direction (unsafe) where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Prelude
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve2d.DegenerateEndpoint (DegenerateEndpoint)
import OpenSolid.VectorCurve2d.DegenerateEndpoint qualified as DegenerateEndpoint

evaluate ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  Float ->
  Vector2d (space @ Unitless)
evaluate Nothing inner Nothing tValue =
  VectorCurve2d.evaluate inner tValue
evaluate (Just degenerateStart) inner Nothing tValue
  | tValue >= DegenerateEndpoint.cutoff degenerateStart = VectorCurve2d.evaluate inner tValue
  | otherwise = DegenerateEndpoint.evaluate degenerateStart inner tValue
evaluate Nothing inner (Just degenerateEnd) tValue
  | tValue <= DegenerateEndpoint.cutoff degenerateEnd = VectorCurve2d.evaluate inner tValue
  | otherwise = DegenerateEndpoint.evaluate degenerateEnd inner tValue
evaluate (Just degenerateStart) inner (Just degenerateEnd) tValue
  | tValue < DegenerateEndpoint.cutoff degenerateStart =
      DegenerateEndpoint.evaluate degenerateStart inner tValue
  | tValue > DegenerateEndpoint.cutoff degenerateEnd =
      DegenerateEndpoint.evaluate degenerateEnd inner tValue
  | otherwise = VectorCurve2d.evaluate inner tValue

evaluateBounds ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  Bounds Unitless ->
  VectorBounds2d (space @ Unitless)
evaluateBounds Nothing inner Nothing tBounds =
  VectorCurve2d.evaluateBounds inner tBounds
evaluateBounds (Just degenerateStart) inner Nothing tBounds = do
  let (Bounds t1 t2) = tBounds
  let tStart = DegenerateEndpoint.cutoff degenerateStart
  if
    | t1 >= tStart -> VectorCurve2d.evaluateBounds inner tBounds
    | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tBounds
    | otherwise ->
        VectorBounds2d.aggregate2
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Bounds t1 tStart))
          (VectorCurve2d.evaluateBounds inner (Bounds tStart t2))
evaluateBounds Nothing inner (Just degenerateEnd) tBounds = do
  let (Bounds t1 t2) = tBounds
  let tEnd = DegenerateEndpoint.cutoff degenerateEnd
  if
    | t2 <= tEnd -> VectorCurve2d.evaluateBounds inner tBounds
    | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tBounds
    | otherwise ->
        VectorBounds2d.aggregate2
          (VectorCurve2d.evaluateBounds inner (Bounds t1 tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Bounds tEnd t2))
evaluateBounds (Just degenerateStart) inner (Just degenerateEnd) tBounds = do
  let (Bounds t1 t2) = tBounds
  let tStart = DegenerateEndpoint.cutoff degenerateStart
  let tEnd = DegenerateEndpoint.cutoff degenerateEnd
  if
    | t1 >= tStart && t2 <= tEnd -> VectorCurve2d.evaluateBounds inner tBounds
    | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tBounds
    | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tBounds
    | t1 >= tStart ->
        VectorBounds2d.aggregate2
          (VectorCurve2d.evaluateBounds inner (Bounds t1 tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Bounds tEnd t2))
    | t2 <= tEnd ->
        VectorBounds2d.aggregate2
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Bounds t1 tStart))
          (VectorCurve2d.evaluateBounds inner (Bounds tStart t2))
    | otherwise ->
        VectorBounds2d.aggregate3
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Bounds t1 tStart))
          (VectorCurve2d.evaluateBounds inner (Bounds tStart tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Bounds tEnd t2))

derivative ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless)
derivative start general end =
  vectorCurve
    (Maybe.map DegenerateEndpoint.derivative start)
    (VectorCurve2d.derivative general)
    (Maybe.map DegenerateEndpoint.derivative end)

vectorCurve ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  VectorCurve2d (space @ Unitless)
vectorCurve start general end = do
  let compiled =
        CompiledFunction.abstract
          (evaluate start general end)
          (evaluateBounds start general end)
  VectorCurve2d.new compiled (derivative start general end)

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
      _ -> vectorCurve degenerateStart normalized degenerateEnd

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
