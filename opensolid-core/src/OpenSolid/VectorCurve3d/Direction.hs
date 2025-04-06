module OpenSolid.VectorCurve3d.Direction (unsafe) where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.DirectionCurve3d (DirectionCurve3d)
import {-# SOURCE #-} OpenSolid.DirectionCurve3d qualified as DirectionCurve3d
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorCurve3d.DegenerateEndpoint (DegenerateEndpoint)
import OpenSolid.VectorCurve3d.DegenerateEndpoint qualified as DegenerateEndpoint

evaluate ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  Float ->
  Vector3d (space @ Unitless)
evaluate Nothing inner Nothing tValue =
  VectorCurve3d.evaluate inner tValue
evaluate (Just degenerateStart) inner Nothing tValue
  | tValue >= DegenerateEndpoint.cutoff degenerateStart = VectorCurve3d.evaluate inner tValue
  | otherwise = DegenerateEndpoint.evaluate degenerateStart inner tValue
evaluate Nothing inner (Just degenerateEnd) tValue
  | tValue <= DegenerateEndpoint.cutoff degenerateEnd = VectorCurve3d.evaluate inner tValue
  | otherwise = DegenerateEndpoint.evaluate degenerateEnd inner tValue
evaluate (Just degenerateStart) inner (Just degenerateEnd) tValue
  | tValue < DegenerateEndpoint.cutoff degenerateStart =
      DegenerateEndpoint.evaluate degenerateStart inner tValue
  | tValue > DegenerateEndpoint.cutoff degenerateEnd =
      DegenerateEndpoint.evaluate degenerateEnd inner tValue
  | otherwise = VectorCurve3d.evaluate inner tValue

evaluateBounds ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  Range Unitless ->
  VectorBounds3d (space @ Unitless)
evaluateBounds Nothing inner Nothing tRange =
  VectorCurve3d.evaluateBounds inner tRange
evaluateBounds (Just degenerateStart) inner Nothing tRange = do
  let (Range t1 t2) = tRange
  let tStart = DegenerateEndpoint.cutoff degenerateStart
  if
    | t1 >= tStart -> VectorCurve3d.evaluateBounds inner tRange
    | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
    | otherwise ->
        VectorBounds3d.aggregate2
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range t1 tStart))
          (VectorCurve3d.evaluateBounds inner (Range tStart t2))
evaluateBounds Nothing inner (Just degenerateEnd) tRange = do
  let (Range t1 t2) = tRange
  let tEnd = DegenerateEndpoint.cutoff degenerateEnd
  if
    | t2 <= tEnd -> VectorCurve3d.evaluateBounds inner tRange
    | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
    | otherwise ->
        VectorBounds3d.aggregate2
          (VectorCurve3d.evaluateBounds inner (Range t1 tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range tEnd t2))
evaluateBounds (Just degenerateStart) inner (Just degenerateEnd) tRange = do
  let (Range t1 t2) = tRange
  let tStart = DegenerateEndpoint.cutoff degenerateStart
  let tEnd = DegenerateEndpoint.cutoff degenerateEnd
  if
    | t1 >= tStart && t2 <= tEnd -> VectorCurve3d.evaluateBounds inner tRange
    | t2 <= tStart -> DegenerateEndpoint.evaluateBounds degenerateStart inner tRange
    | t1 >= tEnd -> DegenerateEndpoint.evaluateBounds degenerateEnd inner tRange
    | t1 >= tStart ->
        VectorBounds3d.aggregate2
          (VectorCurve3d.evaluateBounds inner (Range t1 tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range tEnd t2))
    | t2 <= tEnd ->
        VectorBounds3d.aggregate2
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range t1 tStart))
          (VectorCurve3d.evaluateBounds inner (Range tStart t2))
    | otherwise ->
        VectorBounds3d.aggregate3
          (DegenerateEndpoint.evaluateBounds degenerateStart inner (Range t1 tStart))
          (VectorCurve3d.evaluateBounds inner (Range tStart tEnd))
          (DegenerateEndpoint.evaluateBounds degenerateEnd inner (Range tEnd t2))

derivative ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless)
derivative start general end =
  vectorCurve
    (Maybe.map DegenerateEndpoint.derivative start)
    (VectorCurve3d.derivative general)
    (Maybe.map DegenerateEndpoint.derivative end)

vectorCurve ::
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless) ->
  Maybe (DegenerateEndpoint space) ->
  VectorCurve3d (space @ Unitless)
vectorCurve start general end = do
  let compiled =
        CompiledFunction.abstract
          (evaluate start general end)
          (evaluateBounds start general end)
  VectorCurve3d.new compiled (derivative start general end)

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
      _ -> vectorCurve degenerateStart normalized degenerateEnd

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
