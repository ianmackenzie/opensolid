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
import OpenSolid.Prelude

type Problem dimension units space =
  ( Curve.Exists dimension units space
  , Tolerance units
  , ?curve1 :: Nondegenerate (Curve dimension units space)
  , ?curve2 :: Nondegenerate (Curve dimension units space)
  , ?bisectionTree :: BisectionTree dimension units space
  )

type BisectionTree dimension units space =
  Bisection.Tree
    (Interval Unitless, Interval Unitless)
    (Curve.Segment dimension units space, Curve.Segment dimension units space)

curve1 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
curve1 = ?curve1

curve2 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
curve2 = ?curve2

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
intersections givenCurve1 givenCurve2 = do
  let ?curve1 = givenCurve1
  let ?curve2 = givenCurve2
  let tree1 = Curve.bisectionTree givenCurve1
  let tree2 = Curve.bisectionTree givenCurve2
  let ?bisectionTree = Bisection.pairwise tree1 tree2
  findIntersections

findIntersections :: Problem dimension units space => Maybe (Intersections dimension units space)
findIntersections
  | not (Curve.Nondegenerate.bounds curve1 `intersects` Curve.Nondegenerate.bounds curve2) = Nothing
  | otherwise = do
      let endpointIntersections = findEndpointIntersections
      if List.any IntersectionPoint.isIndistinguishable endpointIntersections
        then findOverlappingIntersections endpointIntersections
        else findNonOverlappingIntersections endpointIntersections

findEndpointIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space)
findEndpointIntersections = do
  let start1 = Curve.Nondegenerate.curvePoint curve1 0.0
  let end1 = Curve.Nondegenerate.curvePoint curve1 1.0
  let start2 = Curve.Nondegenerate.curvePoint curve2 0.0
  let end2 = Curve.Nondegenerate.curvePoint curve2 1.0
  let findPoint curvePoint searchCurve =
        Curve.Nondegenerate.findPoint (CurvePoint.point curvePoint) searchCurve
  let endpoints1On2 = [(p1, p2) | p1 <- [start1, end1], p2 <- findPoint p1 curve2]
  let endpoints2On1 = [(p1, p2) | p2 <- [start2, end2], p1 <- findPoint p2 curve1]
  let location (p1, p2) = (CurvePoint.location p1, CurvePoint.location p2)
  List.sortAndDeduplicateBy location (endpoints1On2 <> endpoints2On1)
    & List.filterMap \(p1, p2) -> do
      continuity <- CurvePoint.continuity p1 p2
      Just (IntersectionPoint continuity (p1, p2))

findOverlappingIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Maybe (Intersections dimension units space)
findOverlappingIntersections endpointIntersections =
  assert (List.all IntersectionPoint.isIndistinguishable endpointIntersections) do
    let (joins, nonJoins) = List.partition IntersectionPoint.isJoin endpointIntersections
    case nonJoins of
      [] -> maybeIntersectionPoints endpointIntersections
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
  Maybe (Intersections dimension units space)
findNonOverlappingIntersections endpointIntersections = do
  let interiorIntersections =
        findInteriorIntersections
          endpointIntersections
          Curve.tangentSolver
          Curve.crossingSolver
  maybeIntersectionPoints (endpointIntersections <> interiorIntersections)

findInteriorIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Curve.Solver dimension units space ->
  Curve.Solver dimension units space ->
  List (IntersectionPoint dimension units space)
findInteriorIntersections
  endpointIntersections
  (Curve.Solver resolveTangent solveTangent)
  (Curve.Solver resolveCrossing solveCrossing) =
    Intersection.solveInterior $
      Intersection.Problem
        { boundaryTangentSubdomains =
            boundarySubdomains (List.filter IntersectionPoint.isTangent endpointIntersections)
        , boundaryCrossingSubdomains =
            boundarySubdomains (List.filter IntersectionPoint.isCrossing endpointIntersections)
        , searchTree = bisectionTree
        , resolveTangent = resolveTangent
        , solveTangent = List.maybe . Bisection.find (solveTangent curve1 curve2)
        , resolveCrossing = resolveCrossing
        , solveCrossing = List.maybe . Bisection.find (solveCrossing curve1 curve2)
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
