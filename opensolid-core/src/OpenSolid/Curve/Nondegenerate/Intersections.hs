{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections (..))
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.Intersection qualified as Intersection
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Prelude

type Problem dimension units space =
  ( Curve.Exists dimension units space
  , Tolerance units
  , ?nondegenerate1 :: Nondegenerate (Curve dimension units space)
  , ?nondegenerate2 :: Nondegenerate (Curve dimension units space)
  , ?bisectionTree :: BisectionTree dimension units space
  )

type BisectionTree dimension units space =
  Bisection.Tree
    (Interval Unitless, Interval Unitless)
    (Curve.Segment dimension units space, Curve.Segment dimension units space)

nondegenerate1 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
nondegenerate1 = ?nondegenerate1

nondegenerate2 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
nondegenerate2 = ?nondegenerate2

curve1 :: Problem dimension units space => Curve dimension units space
curve1 = Nondegenerate.unwrap nondegenerate1

curve2 :: Problem dimension units space => Curve dimension units space
curve2 = Nondegenerate.unwrap nondegenerate2

bisectionTree :: Problem dimension units space => BisectionTree dimension units space
bisectionTree = ?bisectionTree

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe (Intersections dimension units space)
intersections givenNondegenerate1 givenNondegenerate2 = do
  let ?nondegenerate1 = givenNondegenerate1
  let ?nondegenerate2 = givenNondegenerate2
  let tree1 = Curve.bisectionTree givenNondegenerate1
  let tree2 = Curve.bisectionTree givenNondegenerate2
  let ?bisectionTree = Bisection.pairwise tree1 tree2
  findIntersections

findIntersections :: Problem dimension units space => Maybe (Intersections dimension units space)
findIntersections
  | not (Curve.bounds curve1 `intersects` Curve.bounds curve2) = Nothing
  | otherwise = do
      let endpointIntersections = findEndpointIntersections
      if List.any IntersectionPoint.isIndistinguishable endpointIntersections
        then assert (List.all IntersectionPoint.isIndistinguishable endpointIntersections) do
          findOverlappingIntersections endpointIntersections
        else
          findNonOverlappingIntersections
            endpointIntersections
            Curve.tangentSolver
            Curve.crossingSolver

findEndpointIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space)
findEndpointIntersections = do
  let findPoint curve t nondegenerateSearchCurve =
        Curve.Nondegenerate.findPoint (Curve.point curve t) nondegenerateSearchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 nondegenerate2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 nondegenerate1]
  List.uniqueValues (endpoints1On2 <> endpoints2On1)
    & List.filterMap \(t1, t2) -> do
      let p1 = CurvePoint.on nondegenerate1 t1
      let p2 = CurvePoint.on nondegenerate2 t2
      continuity <- CurvePoint.continuity p1 p2
      Just (IntersectionPoint continuity (p1, p2))

findOverlappingIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Maybe (Intersections dimension units space)
findOverlappingIntersections endpointIntersections = do
  let (joins, nonJoins) = List.partition IntersectionPoint.isJoin endpointIntersections
  case nonJoins of
    [] -> maybeIntersectionPoints joins
    NonEmpty candidateEndpoints -> do
      overlapSigns <- Maybe.collect IntersectionPoint.overlapSign candidateEndpoints
      alignment <- NonEmpty.uniqueValue overlapSigns
      let overlappingSegment startIntersectionPoint endIntersectionPoint = do
            let (tStart1, tStart2) = IntersectionPoint.parameterValues startIntersectionPoint
            let (tEnd1, tEnd2) = IntersectionPoint.parameterValues endIntersectionPoint
            (Interval tStart1 tEnd1, Interval tStart2 tEnd2)
      let overlappingSegments segments = OverlappingSegments alignment segments joins
      case NonEmpty.sortBy IntersectionPoint.firstParameterValue candidateEndpoints of
        NonEmpty.Two first second -> do
          let segment = overlappingSegment first second
          Just (overlappingSegments (NonEmpty.one segment))
        NonEmpty.Four first second third fourth -> do
          let segment1 = overlappingSegment first second
          let segment2 = overlappingSegment third fourth
          Just (overlappingSegments (NonEmpty.two segment1 segment2))
        _ -> Nothing

findNonOverlappingIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Curve.Solver dimension units space ->
  Curve.Solver dimension units space ->
  Maybe (Intersections dimension units space)
findNonOverlappingIntersections
  endpointIntersections
  (Curve.Solver resolveTangent solveTangent)
  (Curve.Solver resolveCrossing solveCrossing) =
    maybeIntersectionPoints $
      Intersection.solveNonOverlapping $
        Intersection.Problem
          { boundaryIntersections = endpointIntersections
          , boundaryTangentSubdomains =
              boundarySubdomains (List.filter IntersectionPoint.isTangent endpointIntersections)
          , boundaryCrossingSubdomains =
              boundarySubdomains (List.filter IntersectionPoint.isCrossing endpointIntersections)
          , searchTree = bisectionTree
          , resolveTangent = resolveTangent
          , solveTangent =
              List.maybe . Bisection.find (solveTangent nondegenerate1 nondegenerate2)
          , resolveCrossing = resolveCrossing
          , solveCrossing =
              List.maybe . Bisection.find (solveCrossing nondegenerate1 nondegenerate2)
          }

boundarySubdomains ::
  List (IntersectionPoint dimension units space) ->
  Bag (Interval Unitless, Interval Unitless) (IntersectionPoint dimension units space)
boundarySubdomains =
  Bag.pack \intersectionPoint -> do
    let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
    (Interval.constant t1, Interval.constant t2)

maybeIntersectionPoints ::
  List (IntersectionPoint dimension units space) ->
  Maybe (Intersections dimension units space)
maybeIntersectionPoints [] = Nothing
maybeIntersectionPoints (NonEmpty intersectionPoints) = do
  let sorted = NonEmpty.sortBy IntersectionPoint.parameterValues intersectionPoints
  Just (IntersectionPoints sorted)
