{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate
  ( derivative
  , tangentDirection
  , tangentDirectionValue
  , findPoint
  , intersections
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

derivative ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivative (Nondegenerate curve) = Nondegenerate (Curve.derivative curve)

tangentDirection ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  DirectionCurve dimension space
tangentDirection (Nondegenerate curve) =
  VectorCurve.Nondegenerate.direction (Nondegenerate (Curve.derivative curve))

tangentDirectionValue ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Direction dimension space
tangentDirectionValue (Nondegenerate curve) tValue =
  VectorCurve.Nondegenerate.directionValue (Nondegenerate (Curve.derivative curve)) tValue

data Monotonic = Monotonic deriving (Eq)

findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
findPoint point (Nondegenerate curve) = do
  let isDistant segment = not (point `intersects` Curve.Segment.range segment)
  let resolvedMonotonicity _ segment
        | isDistant segment = Resolved Nothing
        | Curve.Segment.monotonic segment = Resolved (Just Monotonic)
        | Curve.Segment.singular segment = Resolved (Just Monotonic)
        | otherwise = Unresolved
  let evaluate tValue =
        (# Curve.point curve tValue - point, Curve.derivativeValue curve tValue #)
  let resolvedSolution tRange segment
        | isDistant segment = Resolved Nothing
        | Interval.lower tRange == 0.0 && Curve.startPoint curve ~= point = Resolved (Just 0.0)
        | Interval.upper tRange == 1.0 && Curve.endPoint curve ~= point = Resolved (Just 1.0)
        | otherwise = do
            let tSolution = NewtonRaphson.curve evaluate (Interval.midpoint tRange)
            if Tolerance.using Tolerance.unitless (tSolution `intersects` tRange)
              then Resolved (Just tSolution)
              else Unresolved
  Bisection.clusters resolvedMonotonicity (Curve.bisectionTree curve)
    & List.filterMap (\(Monotonic, cluster) -> Bisection.find resolvedSolution cluster)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
intersections = Curve.Nondegenerate.Intersections.intersections
