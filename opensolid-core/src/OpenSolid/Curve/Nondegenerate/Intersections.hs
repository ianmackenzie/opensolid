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
import OpenSolid.Curve.Intersections (Intersections (..))
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
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
  Maybe Intersections
intersections givenCurve1 givenCurve2 = do
  let ?curve1 = givenCurve1
  let ?curve2 = givenCurve2
  let tree1 = Curve.bisectionTree givenCurve1
  let tree2 = Curve.bisectionTree givenCurve2
  let ?bisectionTree = Bisection.pairwise tree1 tree2
  findIntersections

findIntersections :: Problem dimension units space => Maybe Intersections
findIntersections
  | not (Curve.Nondegenerate.bounds curve1 `intersects` Curve.Nondegenerate.bounds curve2) = Nothing
  | otherwise = do
      let endpointIntersections = findEndpointIntersections
      if List.any IntersectionPoint.isIndistinguishable endpointIntersections
        then findOverlappingIntersections endpointIntersections
        else findNonOverlappingIntersections endpointIntersections

findEndpointIntersections :: Problem dimension units space => List IntersectionPoint
findEndpointIntersections = do
  let findPoint curve t searchCurve =
        Curve.Nondegenerate.findPoint (Curve.Nondegenerate.pointOn curve t) searchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 curve2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 curve1]
  List.sortAndDeduplicate (endpoints1On2 <> endpoints2On1)
    & List.filterMap \solution -> do
      continuity <- Curve.Nondegenerate.continuityAt solution (curve1, curve2)
      Just (IntersectionPoint continuity solution)

findOverlappingIntersections ::
  Problem dimension units space =>
  List IntersectionPoint ->
  Maybe Intersections
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
  List IntersectionPoint ->
  Maybe Intersections
findNonOverlappingIntersections endpointIntersections = do
  let interiorIntersections =
        findInteriorIntersections
          endpointIntersections
          Curve.tangentSolver
          Curve.crossingSolver
  maybeIntersectionPoints (endpointIntersections <> interiorIntersections)

findInteriorIntersections ::
  Problem dimension units space =>
  List IntersectionPoint ->
  Curve.Solver dimension units space ->
  Curve.Solver dimension units space ->
  List IntersectionPoint
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
  List IntersectionPoint ->
  Bag (Interval Unitless, Interval Unitless) IntersectionPoint
boundarySubdomains =
  Bag.pack \intersectionPoint -> do
    let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
    (Interval.constant t1, Interval.constant t2)

maybeIntersectionPoints :: List IntersectionPoint -> Maybe Intersections
maybeIntersectionPoints [] = Nothing
maybeIntersectionPoints (NonEmpty intersectionPoints) = do
  let sorted = NonEmpty.sortBy IntersectionPoint.parameterValues intersectionPoints
  Just (IntersectionPoints sorted)
