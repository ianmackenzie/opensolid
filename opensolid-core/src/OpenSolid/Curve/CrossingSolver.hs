{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.CrossingSolver (solver) where

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
import OpenSolid.UvBounds (data UvBounds)
import OpenSolid.UvPoint (data UvPoint)

solver :: Curve.Solver dimension units space
solver = Curve.Solver resolve solve

data Crossing = Crossing

resolve ::
  (CurveExists dimension units space, Tolerance units) =>
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment dimension units space, Curve.Segment dimension units space) ->
  Fuzzy (Maybe Crossing)
resolve _ (segmentA, segmentB)
  | Curve.Segment.areDistinct segmentA segmentB = Resolved Nothing
  | Curve.Segment.haveCrossingTangents segmentA segmentB = Resolved (Just Crossing)
  | otherwise = Unresolved

solve ::
  (CurveExists dimension units space, Tolerance units) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Crossing ->
  (Interval Unitless, Interval Unitless) ->
  (Curve.Segment dimension units space, Curve.Segment dimension units space) ->
  Fuzzy (Maybe IntersectionPoint)
solve curveA curveB Crossing (tRangeA, tRangeB) (segmentA, segmentB) =
  if Curve.Segment.areDistinct segmentA segmentB
    then Resolved Nothing
    else do
      let evaluate (UvPoint tA tB) = do
            let pointA = Curve.Nondegenerate.pointAt tA curveA
            let pointB = Curve.Nondegenerate.pointAt tB curveB
            let displacement = pointB - pointA
            let derivativeA = negate (Curve.Nondegenerate.derivativeAt tA curveA)
            let derivativeB = Curve.Nondegenerate.derivativeAt tB curveB
            (# displacement, derivativeA, derivativeB #)
      UvPoint tA tB <- NewtonRaphson.Surface.solveIn (UvBounds tRangeA tRangeB) evaluate
      let solution = (tA, tB)
      case Curve.Nondegenerate.continuityAt solution (curveA, curveB) of
        Nothing -> Unresolved
        Just Continuity.Crossing -> Resolved (Just (IntersectionPoint.crossing solution))
        Just _ ->
          InternalError.throw $
            "Should have guaranteed by this point that all intersection points are crossing ones"
