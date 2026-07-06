{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.CrossingSolver (solver) where

import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.UvBounds (pattern UvBounds)
import OpenSolid.UvPoint (pattern UvPoint)

solver :: Curve.Solver dimension units space
solver = Curve.Solver resolve solve

data Crossing = Crossing

resolve ::
  (Curve.Exists dimension units space, Tolerance units) =>
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment dimension units space, Curve.Segment dimension units space) ->
  Fuzzy (Maybe Crossing)
resolve _ (segmentA, segmentB)
  | Curve.Segment.areDistinct segmentA segmentB = Resolved Nothing
  | Curve.Segment.haveCrossingTangents segmentA segmentB = Resolved (Just Crossing)
  | otherwise = Unresolved

solve ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Crossing ->
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment dimension units space, Curve.Segment dimension units space) ->
  Fuzzy (Maybe (IntersectionPoint dimension units space))
solve nondegenerateA nondegenerateB Crossing (tRangeA, tRangeB) (segmentA, segmentB) =
  if Curve.Segment.areDistinct segmentA segmentB
    then Resolved Nothing
    else do
      let Nondegenerate curveA = nondegenerateA
      let Nondegenerate curveB = nondegenerateB
      let evaluate (UvPoint tA tB) = do
            let displacement = Curve.point curveB tB - Curve.point curveA tA
            let derivativeA = negate (Curve.derivativeValue curveA tA)
            let derivativeB = Curve.derivativeValue curveB tB
            (# displacement, derivativeA, derivativeB #)
      UvPoint tA tB <- NewtonRaphson.Surface.solveIn (UvBounds tRangeA tRangeB) evaluate
      let p1 = CurvePoint.on nondegenerateA tA
      let p2 = CurvePoint.on nondegenerateB tB
      case CurvePoint.continuity p1 p2 of
        Nothing -> Unresolved
        Just Continuity.G0 -> Resolved (Just (IntersectionPoint.g0 (p1, p2)))
        Just _ ->
          InternalError.throw "Should have guaranteed by this point that continuity is at most G0"
