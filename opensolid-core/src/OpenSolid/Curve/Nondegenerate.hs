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
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.Vector qualified as Vector
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
  ( Curve.Exists dimension units space
  , Direction.Exists dimension space
  , Tolerance units
  ) =>
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
findPoint givenPoint (Nondegenerate curve) = do
  let evaluate tValue =
        (# Curve.point curve tValue - givenPoint, Curve.derivativeValue curve tValue #)
  let isSolution tValue = Curve.point curve tValue ~= givenPoint
  let isDegenerate tValue = Curve.derivativeValue curve tValue ~= Vector.zero
  let endpointSolutions = List.filter isSolution [0.0, 1.0]
  let solveMonotonic tRange = do
        let tMid = Interval.midpoint tRange
        let tSolution = NewtonRaphson.curve evaluate tMid
        if Search.isInterior tSolution tRange && isSolution tSolution
          then Resolved (Just tSolution)
          else Unresolved
  let interiorSolution tRange segment
        | not (givenPoint `intersects` Curve.Segment.range segment) = Resolved Nothing
        | otherwise = do
            let isMonotonic = Curve.Segment.monotonic segment
            let isSmall = SearchDomain.isSmall tRange
            let endpointSolution = List.find (Number.includedIn tRange) endpointSolutions
            let hasEndpointSolution = endpointSolution /= Nothing
            if
              | isMonotonic && hasEndpointSolution -> Resolved Nothing
              | isSmall, Just tValue <- endpointSolution, isDegenerate tValue -> Resolved Nothing
              | isMonotonic -> solveMonotonic tRange
              | otherwise -> Unresolved
  let isDuplicate (tRange1, _) (tRange2, _) = SearchDomain.overlapping tRange1 tRange2
  let interiorSolutions =
        Search.exclusive interiorSolution isDuplicate (Curve.searchTree curve)
          & List.map Pair.second
  List.sort (endpointSolutions <> interiorSolutions)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
intersections = Curve.Nondegenerate.Intersections.intersections
