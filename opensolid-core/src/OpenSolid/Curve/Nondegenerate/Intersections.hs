{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections (..))
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.CurvePoint (CurvePoint)
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
import OpenSolid.Tolerance qualified as Tolerance
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
      let endpointSolutions = findEndpointSolutions
      case findOverlappingSegments endpointSolutions of
        Just (alignment, segments) -> Just (OverlappingSegments alignment segments)
        Nothing -> do
          let interiorIntersectionPoints = findInteriorSolutions endpointSolutions
          case List.sortBy IntersectionPoint.parameterValues (endpointSolutions <> interiorIntersectionPoints) of
            NonEmpty intersectionPoints -> Just (IntersectionPoints intersectionPoints)
            [] -> Nothing

findEndpointSolutions ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space)
findEndpointSolutions = do
  let findPoint curve t nondegenerateSearchCurve =
        Curve.Nondegenerate.findPoint (Curve.point curve t) nondegenerateSearchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 nondegenerate2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 nondegenerate1]
  List.uniqueValues (endpoints1On2 <> endpoints2On1)
    & List.map \(t1, t2) -> do
      let p1 = CurvePoint.on nondegenerate1 t1
      let p2 = CurvePoint.on nondegenerate2 t2
      let tangentDirection1 = CurvePoint.tangentDirection p1
      let tangentDirection2 = CurvePoint.tangentDirection p2
      if
        | tangentDirection1 ~= tangentDirection2 -> IntersectionPoint.Tangent Positive (p1, p2)
        | tangentDirection1 ~= -tangentDirection2 -> IntersectionPoint.Tangent Negative (p1, p2)
        | otherwise -> IntersectionPoint.Crossing (p1, p2)

findInteriorSolution ::
  ( Problem dimension units space
  , NewtonRaphson.Surface.Solver solveDimension solveUnits solveSpace
  ) =>
  NewtonRaphson.Surface.Function solveDimension solveUnits solveSpace ->
  Interval Unitless ->
  Interval Unitless ->
  Maybe (CurvePoint dimension units space, CurvePoint dimension units space)
findInteriorSolution evaluate tRange1 tRange2 =
  case NewtonRaphson.Surface.solveIn (UvBounds tRange1 tRange2) evaluate of
    Unresolved -> Nothing
    Resolved (UvPoint t1 t2) ->
      if Curve.point curve1 t1 ~= Curve.point curve2 t2
        then Just (CurvePoint.on nondegenerate1 t1, CurvePoint.on nondegenerate2 t2)
        else Nothing

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

findInteriorSolutions ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  List (IntersectionPoint dimension units space)
findInteriorSolutions endpointSolutions = do
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
            let singleEndpointSolution =
                  case List.filter isLocal endpointSolutions of
                    List.One endpointSolution -> Just endpointSolution
                    [] -> Nothing
                    List.TwoOrMore -> Nothing
            let hasCrossingEndpointSolution = case singleEndpointSolution of
                  Just IntersectionPoint.Crossing{} -> True
                  Just IntersectionPoint.Tangent{} -> False
                  Nothing -> False
            let hasContinuation = case singleEndpointSolution of
                  Just intersectionPoint -> do
                    let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
                    if
                      | (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0) ->
                          DirectionBounds.areDistinct tangentDirectionRange1 tangentDirectionRange2
                      | (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0) ->
                          DirectionBounds.areDistinct tangentDirectionRange1 -tangentDirectionRange2
                      | otherwise -> False
                  Nothing -> False
            let hasTangentEndpointSolution = case singleEndpointSolution of
                  Just IntersectionPoint.Tangent{} -> True
                  Just IntersectionPoint.Crossing{} -> False
                  Nothing -> False
            let hasDegenerateEndpointSolution = case singleEndpointSolution of
                  Just intersectionPoint -> do
                    let (p1, p2) = IntersectionPoint.curvePoints intersectionPoint
                    CurvePoint.isDegenerate p1 || CurvePoint.isDegenerate p2
                  Nothing -> False
            let isSmall = SearchDomain.isSmall tRange1
            if
              | hasContinuation -> Resolved Nothing
              | isCrossing && hasCrossingEndpointSolution -> Resolved Nothing
              | curvatureVectorsAreDistinct && hasTangentEndpointSolution -> Resolved Nothing
              | isSmall && hasDegenerateEndpointSolution -> Resolved Nothing
              | isCrossing ->
                  case findInteriorSolution evaluateCrossing tRange1 tRange2 of
                    Just points -> Resolved (Just (IntersectionPoint.Crossing points))
                    Nothing -> Unresolved
              | curvatureVectorsAreDistinct ->
                  case findInteriorSolution evaluateTangent tRange1 tRange2 of
                    Just points -> do
                      let (p1, p2) = points
                      let tangent1 = CurvePoint.tangentDirection p1
                      let tangent2 = CurvePoint.tangentDirection p2
                      Tolerance.using Tolerance.unitless do
                        if
                          | tangent1 ~= tangent2 ->
                              Resolved (Just (IntersectionPoint.Tangent Positive points))
                          | tangent1 ~= -tangent2 ->
                              Resolved (Just (IntersectionPoint.Tangent Negative points))
                          | otherwise -> Unresolved
                    Nothing -> Unresolved
              | otherwise -> Unresolved
  let isDuplicate (tRange1, _) (tRange2, _) = SearchDomain.overlapping tRange1 tRange2
  List.map Pair.second (Search.exclusive interiorIntersectionPoint isDuplicate searchTree)

findOverlappingSegments ::
  Problem dimension units space =>
  List (IntersectionPoint dimension units space) ->
  Maybe (Sign, NonEmpty (Interval Unitless, Interval Unitless))
findOverlappingSegments [] = Nothing
findOverlappingSegments (NonEmpty endpointSolutions) = do
  let alignmentAt endpointSolution = case endpointSolution of
        IntersectionPoint.Tangent alignment _ -> Just alignment
        IntersectionPoint.Crossing{} -> Nothing
  endpointSolutionAlignments <- Maybe.collect alignmentAt endpointSolutions
  alignment <- NonEmpty.uniqueValue endpointSolutionAlignments
  let isContinuation intersectionPoint = do
        let (t1, t2) = IntersectionPoint.parameterValues intersectionPoint
        case alignment of
          Positive -> (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0)
          Negative -> (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0)
  let candidateEndpoints =
        endpointSolutions
          & NonEmpty.filter (not . isContinuation)
          & List.sortBy IntersectionPoint.firstParameterValue
  let isOverlappingSegment startIntersectionPoint endIntersectionPoint = do
        let tStart1 = IntersectionPoint.firstParameterValue startIntersectionPoint
        let tEnd1 = IntersectionPoint.firstParameterValue endIntersectionPoint
        let tRange1 = Interval tStart1 tEnd1
        let tValues1 = Interval.sampleValues tRange1
        let samplePoints1 = NonEmpty.map (Curve.point curve1) tValues1
        NonEmpty.all (intersects curve2) samplePoints1
  let overlappingSegment startIntersectionPoint endIntersectionPoint = do
        let (tStart1, tStart2) = IntersectionPoint.parameterValues startIntersectionPoint
        let (tEnd1, tEnd2) = IntersectionPoint.parameterValues endIntersectionPoint
        (Interval tStart1 tEnd1, Interval tStart2 tEnd2)
  case candidateEndpoints of
    [first, second] ->
      if isOverlappingSegment first second
        then Just (alignment, NonEmpty.one (overlappingSegment first second))
        else Nothing
    [first, second, third, fourth] ->
      if isOverlappingSegment first second
        then do
          let segment1 = overlappingSegment first second
          let segment2 = overlappingSegment third fourth
          Just (alignment, NonEmpty.two segment1 segment2)
        else Nothing
    _ -> Nothing
