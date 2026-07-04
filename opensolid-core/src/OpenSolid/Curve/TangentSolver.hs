{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.TangentSolver
  ( lengthScale
  , areDistinctOrCrossing
  , secondDerivativeRange
  , solve
  )
where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (pattern UvBounds)
import OpenSolid.UvPoint (pattern UvPoint)

lengthScale ::
  Curve.Exists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space ->
  Quantity units
lengthScale curveA curveB = do
  let scaleA = Bounds.diameter (Curve.bounds curveA)
  let scaleB = Bounds.diameter (Curve.bounds curveB)
  Quantity.sqrt_ (scaleA ?*? scaleB)

areDistinctOrCrossing ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Curve.Segment dimension units space ->
  Curve.Segment dimension units space ->
  Bool
areDistinctOrCrossing segmentA segmentB =
  Curve.Segment.areDistinct segmentA segmentB
    || Curve.Segment.haveCrossingTangents segmentA segmentB

secondDerivativeRange ::
  Interval units ->
  Interval units ->
  Interval units ->
  Interval units ->
  Interval (Unitless ?/? units)
secondDerivativeRange dxdt dydt d2xdt2 d2ydt2 = Units.simplify do
  (d2ydt2 ?*? dxdt - d2xdt2 ?*? dydt) ?/? (dxdt ?*? dxdt ?*? dxdt)

solve ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Interval Unitless ->
  Interval Unitless ->
  NewtonRaphson.Surface.Function 2 units Void ->
  Fuzzy (Maybe (IntersectionPoint dimension units space))
solve nondegenerateA nondegenerateB tRangeA tRangeB function = do
  UvPoint tA tB <- NewtonRaphson.Surface.solveIn (UvBounds tRangeA tRangeB) function
  let p1 = CurvePoint.on nondegenerateA tA
  let p2 = CurvePoint.on nondegenerateB tB
  case CurvePoint.continuity p1 p2 of
    Nothing -> Unresolved
    Just continuity -> case continuity of
      Continuity.G0 -> Unresolved
      Continuity.G1 _ -> Resolved (Just (IntersectionPoint continuity (p1, p2)))
      Continuity.G2 _ ->
        InternalError.throw "Should have guaranteed by this point that curvatures are not equal"
