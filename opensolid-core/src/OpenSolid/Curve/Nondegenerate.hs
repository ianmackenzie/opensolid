{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate
  ( point
  , bounds
  , derivative
  , derivativeValue
  , secondDerivativeValue
  , tangentDirection
  , bisectionTree
  , findPoint
  , intersections
  )
where

import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds)
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

{-# INLINE point #-}
point ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Point dimension units space
point (Nondegenerate curve) parameterValue = Curve.point curve parameterValue

bounds ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Bounds dimension units space
bounds (Nondegenerate curve) = Curve.bounds curve

derivative ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivative (Nondegenerate curve) = Nondegenerate (Curve.derivative curve)

{-# INLINE derivativeValue #-}
derivativeValue ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Vector dimension units space
derivativeValue (Nondegenerate curve) parameterValue =
  Curve.derivativeValue curve parameterValue

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Vector dimension units space
secondDerivativeValue (Nondegenerate curve) parameterValue =
  Curve.secondDerivativeValue curve parameterValue

tangentDirection ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Direction dimension space
tangentDirection curve tValue =
  VectorCurve.Nondegenerate.direction (derivative curve) tValue

bisectionTree ::
  Nondegenerate (Curve dimension units space) ->
  Curve.BisectionTree dimension units space
bisectionTree = Curve.bisectionTree

data Monotonic = Monotonic deriving (Eq)

findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
findPoint givenPoint givenCurve = do
  let endpointSolutions = [t | t <- [0.0, 1.0], point givenCurve t ~= givenPoint]
  let endpointSolutionSet = Bag.pack Interval.constant endpointSolutions
  let isDistant segment = not (givenPoint `intersects` Curve.Segment.range segment)
  let resolvedMonotonicity _ segment
        | isDistant segment = Resolved Nothing
        | Curve.Segment.isMonotonic segment = Resolved (Just Monotonic)
        | Curve.Segment.isDegenerate segment = Resolved (Just Monotonic)
        | otherwise = Unresolved
  let evaluate tValue =
        (# point givenCurve tValue - givenPoint, derivativeValue givenCurve tValue #)
  let resolvedSolution Monotonic tRange segment
        | isDistant segment = Resolved Nothing
        | otherwise = Fuzzy.map Just (NewtonRaphson.Curve.solveIn tRange evaluate)
  let clusters =
        Curve.bisectionTree givenCurve
          & Bisection.clusters endpointSolutionSet resolvedMonotonicity
  let interiorSolutions = List.filterMap (Bisection.find resolvedSolution) clusters
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
