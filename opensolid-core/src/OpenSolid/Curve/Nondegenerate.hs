{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate
  ( derivative
  , tangentDirection
  , tangentDirectionValue
  , findPoint
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Queue qualified as Queue
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.Set qualified as Set
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

findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
findPoint point (Nondegenerate curve) = do
  let findCandidateSegments tree = do
        let Curve.Tree tRange segment left right = tree
        if
          | not (point `intersects` Curve.Segment.range segment) -> Nothing
          | Curve.Segment.monotonic segment -> Just (Set.singleton tRange tree)
          | Curve.Segment.singular segment -> Just (Set.singleton tRange tree)
          | otherwise -> findCandidateSegments left <> findCandidateSegments right
  case findCandidateSegments (Curve.tree curve) of
    Nothing -> []
    Just candidateSegments -> do
      let clusters = Set.clusters SearchDomain.touching (\_ _ -> True) candidateSegments
      let f tValue = Curve.point curve tValue - point
      let f' tValue = Curve.derivativeValue curve tValue
      let evaluate tValue = (# f tValue, f' tValue #)
      let findSolution queue = do
            (subtree, remaining) <- Queue.pop queue
            let Curve.Tree tRange segment left right = subtree
            let Interval t1 t2 = tRange
            if
              | not (point `intersects` Curve.Segment.range segment) -> findSolution remaining
              | t1 == 0.0 && Curve.startPoint curve ~= point -> Just 0.0
              | t2 == 1.0 && Curve.endPoint curve ~= point -> Just 1.0
              | otherwise -> do
                  let tSolution = NewtonRaphson.curve evaluate (Interval.midpoint tRange)
                  if Tolerance.using Tolerance.unitless (tSolution `intersects` tRange)
                    then Just tSolution
                    else findSolution (remaining & Queue.push left & Queue.push right)
      clusters
        & NonEmpty.map Queue.fromNonEmpty
        & NonEmpty.filterMap findSolution

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
intersections = Curve.Nondegenerate.Intersections.intersections
