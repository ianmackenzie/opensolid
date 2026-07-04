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
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Number qualified as Number
import OpenSolid.Point (Point)
import OpenSolid.Prelude
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
findPoint point nondegenerateCurve = do
  let Nondegenerate curve = nondegenerateCurve
  let endpointSolutions = [t | t <- [0.0, 1.0], Curve.point curve t ~= point]
  let isDistant segment = not (point `intersects` Curve.Segment.range segment)
  let resolvedMonotonicity _ segment
        | isDistant segment = Resolved Nothing
        | Curve.Segment.isMonotonic segment = Resolved (Just Monotonic)
        | Curve.Segment.isDegenerate segment = Resolved (Just Monotonic)
        | otherwise = Unresolved
  let areNeighbours Monotonic Monotonic = True
  let evaluate tValue =
        (# Curve.point curve tValue - point, Curve.derivativeValue curve tValue #)
  let resolvedSolution Monotonic tRange segment
        | isDistant segment = Resolved Nothing
        | otherwise = Fuzzy.map Just (NewtonRaphson.Curve.solveIn tRange evaluate)
  let hasEndpointSolution tRange = List.any (Number.includedIn tRange) endpointSolutions
  let clusterHasEndpointSolution cluster =
        NonEmpty.any (\(Monotonic, tree) -> hasEndpointSolution (Bisection.subdomain tree)) cluster
  let clusters =
        Curve.bisectionTree nondegenerateCurve
          & Bisection.clusters resolvedMonotonicity areNeighbours
  let interiorClusters = List.filter (not . clusterHasEndpointSolution) clusters
  let interiorSolutions = List.filterMap (Bisection.find resolvedSolution) interiorClusters
  List.sort (endpointSolutions <> interiorSolutions)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe (Intersections dimension units space)
intersections = Curve.Nondegenerate.Intersections.intersections
