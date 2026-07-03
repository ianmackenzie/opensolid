{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections (..))
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.SearchTree qualified as SearchTree
import OpenSolid.UvBounds (pattern UvBounds)
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds qualified as VectorBounds

type Problem dimension units space =
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  , ?nondegenerate1 :: Nondegenerate (Curve dimension units space)
  , ?nondegenerate2 :: Nondegenerate (Curve dimension units space)
  )

nondegenerate1 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
nondegenerate1 = ?nondegenerate1

nondegenerate2 :: Problem dimension units space => Nondegenerate (Curve dimension units space)
nondegenerate2 = ?nondegenerate2

curve1 :: Problem dimension units space => Curve dimension units space
curve1 = Nondegenerate.unwrap nondegenerate1

curve2 :: Problem dimension units space => Curve dimension units space
curve2 = Nondegenerate.unwrap nondegenerate2

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
  findIntersections

findIntersections :: Problem dimension units space => Maybe (Intersections dimension units space)
findIntersections
  | not (Curve.bounds curve1 `intersects` Curve.bounds curve2) = Nothing
  | otherwise = do
      let endpointIntersections = findEndpointIntersections
      let (joins, nonJoins) = List.partition IntersectionPoint.isJoin endpointIntersections
      case findOverlappingSegments nonJoins of
        Just (alignment, segments) -> Just (OverlappingSegments alignment segments joins)
        Nothing -> do
          let interiorIntersections = findInteriorIntersections endpointIntersections
          let allIntersections = endpointIntersections <> interiorIntersections
          case List.sortBy IntersectionPoint.parameterValues allIntersections of
            NonEmpty intersectionPoints -> Just (IntersectionPoints intersectionPoints)
            [] -> Nothing

validateIntersection ::
  Problem dimension units space =>
  Number ->
  Number ->
  Maybe (IntersectionPoint dimension units space)
validateIntersection t1 t2 = do
  let p1 = CurvePoint.on nondegenerate1 t1
  let p2 = CurvePoint.on nondegenerate2 t2
  continuity <- CurvePoint.continuity p1 p2
  Just (IntersectionPoint continuity (p1, p2))

findEndpointIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space)
findEndpointIntersections = do
  let findPoint curve t nondegenerateSearchCurve =
        Curve.Nondegenerate.findPoint (Curve.point curve t) nondegenerateSearchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 nondegenerate2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 nondegenerate1]
  List.uniqueValues (endpoints1On2 <> endpoints2On1)
    & List.filterMap \(t1, t2) -> validateIntersection t1 t2

findInteriorIntersection ::
  ( Problem dimension units space
  , NewtonRaphson.Surface.Solver solveDimension solveUnits solveSpace
  ) =>
  NewtonRaphson.Surface.Function solveDimension solveUnits solveSpace ->
  Interval Unitless ->
  Interval Unitless ->
  Maybe (IntersectionPoint dimension units space)
findInteriorIntersection evaluate tRange1 tRange2 =
  case NewtonRaphson.Surface.solveIn (UvBounds tRange1 tRange2) evaluate of
    Unresolved -> Nothing
    Resolved (UvPoint t1 t2) -> validateIntersection t1 t2

evaluateCrossing ::
  Problem dimension units space =>
  NewtonRaphson.Surface.Function dimension units space
evaluateCrossing (UvPoint t1 t2) = do
  let displacement = Curve.point curve2 t2 - Curve.point curve1 t1
  let uDerivative = negate (Curve.derivativeValue curve1 t1)
  let vDerivative = Curve.derivativeValue curve2 t2
  (# displacement, uDerivative, vDerivative #)

evaluateTangent ::
  Problem dimension units space =>
  NewtonRaphson.Surface.Function 2 (units ?*? units) Void
evaluateTangent (UvPoint u v) = do
  let f = Curve.point curve1 u
  let g = Curve.point curve2 v
  let d = g - f
  let fu = Curve.derivativeValue curve1 u
  let fuu = Curve.secondDerivativeValue curve1 u
  let gv = Curve.derivativeValue curve2 v
  let gvv = Curve.secondDerivativeValue curve2 v
  let x = d `dot_` fu
  let y = d `dot_` gv
  let xu = negate (fu `dot_` fu) + d `dot_` fuu
  let xv = fu `dot_` gv
  let yu = negate xv
  let yv = gv `dot_` gv + d `dot_` gvv
  (# Vector2D x y, Vector2D xu yu, Vector2D xv yv #)

findInteriorIntersections ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  List (IntersectionPoint dimension units space)
findInteriorIntersections endpointIntersections = do
  let searchTree1 = Curve.searchTree nondegenerate1
  let searchTree2 = Curve.searchTree nondegenerate2
  let searchTree = SearchTree.pairwise (,) searchTree1 searchTree2
  let interiorIntersectionPoint (tRange1, tRange2) (segment1, segment2)
        | Curve.Segment.areDistinct segment1 segment2 = Resolved Nothing
        | otherwise = do
            let tangentDirectionRange1 = Curve.Segment.tangentDirectionRange segment1
            let tangentDirectionRange2 = Curve.Segment.tangentDirectionRange segment2
            let curvatureVectorRange1_ = Curve.Segment.curvatureVectorRange_ segment1
            let curvatureVectorRange2_ = Curve.Segment.curvatureVectorRange_ segment2
            let isCrossing =
                  DirectionBounds.areIndependent tangentDirectionRange1 tangentDirectionRange2
            let curvatureVectorsAreDistinct =
                  VectorBounds.areDistinct curvatureVectorRange1_ curvatureVectorRange2_
            let isLocal intersectionPoint = do
                  let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
                  Interval.member t1 tRange1 && Interval.member t2 tRange2
            let singleEndpointIntersection =
                  case List.filter isLocal endpointIntersections of
                    List.One endpointIntersection -> Just endpointIntersection
                    [] -> Nothing
                    List.TwoOrMore -> Nothing
            let hasCrossingEndpointIntersection = case singleEndpointIntersection of
                  Just intersectionPoint ->
                    IntersectionPoint.continuity intersectionPoint == Continuity.G0
                  Nothing -> False
            let hasContinuation = case singleEndpointIntersection of
                  Just intersectionPoint -> do
                    let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
                    if
                      | (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0) ->
                          DirectionBounds.areDistinct tangentDirectionRange1 tangentDirectionRange2
                      | (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0) ->
                          DirectionBounds.areDistinct tangentDirectionRange1 -tangentDirectionRange2
                      | otherwise -> False
                  Nothing -> False
            let hasTangentEndpointIntersection = case singleEndpointIntersection of
                  Just intersectionPoint ->
                    IntersectionPoint.continuity intersectionPoint > Continuity.G0
                  Nothing -> False
            let hasDegenerateEndpointIntersection = case singleEndpointIntersection of
                  Just intersectionPoint -> do
                    let (p1, p2) = IntersectionPoint.curvePoints intersectionPoint
                    CurvePoint.isDegenerate p1 || CurvePoint.isDegenerate p2
                  Nothing -> False
            let isSmall = SearchDomain.isSmall tRange1
            if
              | hasContinuation -> Resolved Nothing
              | isCrossing && hasCrossingEndpointIntersection -> Resolved Nothing
              | curvatureVectorsAreDistinct && hasTangentEndpointIntersection -> Resolved Nothing
              | isSmall && hasDegenerateEndpointIntersection -> Resolved Nothing
              | isCrossing ->
                  case findInteriorIntersection evaluateCrossing tRange1 tRange2 of
                    Just intersectionPoint -> Resolved (Just intersectionPoint)
                    Nothing -> Unresolved
              | curvatureVectorsAreDistinct ->
                  case findInteriorIntersection evaluateTangent tRange1 tRange2 of
                    Just intersectionPoint -> Resolved (Just intersectionPoint)
                    Nothing -> Unresolved
              | otherwise -> Unresolved
  let isDuplicate (tRange1, _) (tRange2, _) = SearchDomain.overlapping tRange1 tRange2
  List.map Pair.second (Search.exclusive interiorIntersectionPoint isDuplicate searchTree)

findOverlappingSegments ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Maybe (Sign, NonEmpty (Interval Unitless, Interval Unitless))
findOverlappingSegments [] = Nothing
findOverlappingSegments (NonEmpty candidateEndpoints) = do
  overlapSigns <- Maybe.collect IntersectionPoint.overlapSign candidateEndpoints
  alignment <- NonEmpty.uniqueValue overlapSigns
  let overlappingSegment startIntersectionPoint endIntersectionPoint = do
        let (tStart1, tStart2) = IntersectionPoint.parameterValues startIntersectionPoint
        let (tEnd1, tEnd2) = IntersectionPoint.parameterValues endIntersectionPoint
        (Interval tStart1 tEnd1, Interval tStart2 tEnd2)
  case NonEmpty.sortBy IntersectionPoint.firstParameterValue candidateEndpoints of
    NonEmpty.Two first second ->
      Just (alignment, NonEmpty.one (overlappingSegment first second))
    NonEmpty.Four first second third fourth -> do
      let segment1 = overlappingSegment first second
      let segment2 = overlappingSegment third fourth
      Just (alignment, NonEmpty.two segment1 segment2)
    _ -> Nothing
