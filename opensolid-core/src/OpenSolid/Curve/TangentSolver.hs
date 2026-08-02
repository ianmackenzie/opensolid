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
import OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (data UvBounds)
import OpenSolid.UvPoint (data UvPoint)

lengthScale ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  Curve dimension units space ->
  Quantity units
lengthScale curveA curveB = do
  let scaleA = Bounds.diameter (Curve.bounds curveA)
  let scaleB = Bounds.diameter (Curve.bounds curveB)
  Quantity.sqrt_ (scaleA ?*? scaleB)

areDistinctOrCrossing ::
  (CurveExists dimension units space, Tolerance units) =>
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
  (CurveExists dimension units space, Tolerance units) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Interval Unitless ->
  Interval Unitless ->
  NewtonRaphson.Surface.Function 2 units Void ->
  Fuzzy (Maybe IntersectionPoint)
solve nondegenerateA nondegenerateB tRangeA tRangeB function = do
  UvPoint tA tB <- NewtonRaphson.Surface.solveIn (UvBounds tRangeA tRangeB) function
  let solution = (tA, tB)
  case Curve.Nondegenerate.continuityAt solution (nondegenerateA, nondegenerateB) of
    Nothing -> Unresolved
    Just continuity -> case continuity of
      Continuity.Crossing -> Unresolved
      Continuity.Tangent sign -> Resolved (Just (IntersectionPoint.tangent sign solution))
      Continuity.Indistinguishable _ ->
        InternalError.throw "Should have guaranteed by this point that curvatures are not equal"
