{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Nondegenerate.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint
  ( IntersectionPoint (IntersectionPoint)
  , Kind (Crossing, Tangent)
  )
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections (..))
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.SearchTree qualified as SearchTree
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds qualified as VectorBounds

type Problem dimension units space =
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
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
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
intersections givenNondegenerate1 givenNondegenerate2 = do
  let ?nondegenerate1 = givenNondegenerate1
  let ?nondegenerate2 = givenNondegenerate2
  findIntersections

findIntersections :: Problem dimension units space => Maybe Intersections
findIntersections
  | not (Curve.bounds curve1 `intersects` Curve.bounds curve2) = Nothing
  | otherwise = do
      let endpointSolutions = findEndpointSolutions
      case findOverlappingSegments endpointSolutions of
        Just (alignment, segments) -> Just (OverlappingSegments alignment segments)
        Nothing -> do
          let interiorIntersectionPoints = findInteriorSolutions endpointSolutions
          case List.sort (endpointSolutions <> interiorIntersectionPoints) of
            NonEmpty intersectionPoints -> Just (IntersectionPoints intersectionPoints)
            [] -> Nothing

findEndpointSolutions :: Problem dimension units space => List IntersectionPoint
findEndpointSolutions = do
  let findPoint curve t nondegenerateSearchCurve =
        Curve.Nondegenerate.findPoint (Curve.point curve t) nondegenerateSearchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 nondegenerate2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 nondegenerate1]
  List.uniqueValues (endpoints1On2 <> endpoints2On1)
    & List.map \(t1, t2) -> IntersectionPoint (endpointSolutionKind t1 t2) t1 t2

endpointSolutionKind :: Problem dimension units space => Number -> Number -> IntersectionPoint.Kind
endpointSolutionKind t1 t2 = do
  let tangentDirection1 = Curve.Nondegenerate.tangentDirectionValue nondegenerate1 t1
  let tangentDirection2 = Curve.Nondegenerate.tangentDirectionValue nondegenerate2 t2
  if
    | tangentDirection1 ~= tangentDirection2 -> Tangent Positive
    | tangentDirection1 ~= -tangentDirection2 -> Tangent Negative
    | otherwise -> Crossing

findInteriorSolution ::
  (Problem dimension units space, NewtonRaphson.Surface solveDimension solveUnits solveSpace) =>
  NewtonRaphson.EvaluateSurface solveDimension solveUnits solveSpace ->
  Interval Unitless ->
  Interval Unitless ->
  Maybe (Number, Number)
findInteriorSolution evaluate tRange1 tRange2 = do
  let uvPoint0 = UvPoint (Interval.midpoint tRange1) (Interval.midpoint tRange2)
  let UvPoint t1 t2 = NewtonRaphson.surface evaluate uvPoint0
  let isInterior1 = Search.isInterior t1 tRange1
  let isInterior2 = Search.isInterior t2 tRange2
  let pointsAreEqual = Curve.point curve1 t1 ~= Curve.point curve2 t2
  if isInterior1 && isInterior2 && pointsAreEqual then Just (t1, t2) else Nothing

findInteriorSolutions ::
  Problem dimension units space =>
  List IntersectionPoint ->
  List IntersectionPoint
findInteriorSolutions endpointSolutions = do
  let nonzero1 = Nondegenerate.interior nondegenerate1
  let nonzero2 = Nondegenerate.interior nondegenerate2
  let searchTree = SearchTree.pairwise (,) (Curve.searchTree curve1) (Curve.searchTree curve2)
  let evaluateCrossing (UvPoint t1 t2) = do
        let displacement = Curve.point curve2 t2 - Curve.point curve1 t1
        let uDerivative = negate (Curve.derivativeValue curve1 t1)
        let vDerivative = Curve.derivativeValue curve2 t2
        (# displacement, uDerivative, vDerivative #)
  let evaluateTangent (UvPoint u v) = do
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
  let interiorIntersectionPoint (tRange1, tRange2) (segment1, segment2)
        | not (Curve.Segment.range segment1 `intersects` Curve.Segment.range segment2) =
            Resolved Nothing
        | otherwise = do
            let tangentDirectionRange1 = Curve.Segment.tangentDirectionRange segment1
            let tangentDirectionRange2 = Curve.Segment.tangentDirectionRange segment2
            let curvatureVectorRange1_ = Curve.Segment.curvatureVectorRange_ segment1
            let curvatureVectorRange2_ = Curve.Segment.curvatureVectorRange_ segment2
            let isCrossing =
                  DirectionBounds.areIndependent tangentDirectionRange1 tangentDirectionRange2
            let curvatureVectorsAreDistinct =
                  VectorBounds.areDistinct curvatureVectorRange1_ curvatureVectorRange2_
            let isLocal (IntersectionPoint _ t1 t2) =
                  Interval.member t1 tRange1 && Interval.member t2 tRange2
            let singleEndpointSolution =
                  case List.filter isLocal endpointSolutions of
                    List.One endpointSolution -> Just endpointSolution
                    [] -> Nothing
                    List.TwoOrMore -> Nothing
            let hasCrossingEndpointSolution = case singleEndpointSolution of
                  Just endpointSolution -> case IntersectionPoint.kind endpointSolution of
                    Crossing -> True
                    Tangent _ -> False
                  Nothing -> False
            let hasContinuation = case singleEndpointSolution of
                  Just (IntersectionPoint _ t1 t2)
                    | (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0) ->
                        DirectionBounds.areDistinct tangentDirectionRange1 tangentDirectionRange2
                    | (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0) ->
                        DirectionBounds.areDistinct tangentDirectionRange1 -tangentDirectionRange2
                    | otherwise -> False
                  Nothing -> False
            let hasTangentEndpointSolution = case singleEndpointSolution of
                  Just endpointSolution -> case IntersectionPoint.kind endpointSolution of
                    Tangent _ -> True
                    Crossing -> False
                  Nothing -> False
            let hasDegenerateEndpointSolution = case singleEndpointSolution of
                  Just (IntersectionPoint _ t1 t2) ->
                    Curve.derivativeValue curve1 t1 ~= Vector.zero
                      || Curve.derivativeValue curve2 t2 ~= Vector.zero
                  Nothing -> False
            let isSmall = SearchDomain.isSmall tRange1
            if
              | hasContinuation -> Resolved Nothing
              | isCrossing && hasCrossingEndpointSolution -> Resolved Nothing
              | curvatureVectorsAreDistinct && hasTangentEndpointSolution -> Resolved Nothing
              | isSmall && hasDegenerateEndpointSolution -> Resolved Nothing
              | isCrossing ->
                  case findInteriorSolution evaluateCrossing tRange1 tRange2 of
                    Just (t1, t2) -> Resolved (Just (IntersectionPoint.crossing t1 t2))
                    Nothing -> Unresolved
              | curvatureVectorsAreDistinct ->
                  case findInteriorSolution evaluateTangent tRange1 tRange2 of
                    Just (t1, t2) -> do
                      let tangentVector1 = Curve.Nonzero.tangentDirectionValue nonzero1 t1
                      let tangentVector2 = Curve.Nonzero.tangentDirectionValue nonzero2 t2
                      Tolerance.using Tolerance.unitless do
                        if
                          | tangentVector1 ~= tangentVector2 ->
                              Resolved (Just (IntersectionPoint.tangent Positive t1 t2))
                          | tangentVector1 ~= -tangentVector2 ->
                              Resolved (Just (IntersectionPoint.tangent Negative t1 t2))
                          | otherwise -> Unresolved
                    Nothing -> Unresolved
              | otherwise -> Unresolved
  let isDuplicate (tRange1, _) (tRange2, _) = SearchDomain.overlapping tRange1 tRange2
  List.map Pair.second (Search.exclusive interiorIntersectionPoint isDuplicate searchTree)

findOverlappingSegments ::
  Problem dimension units space =>
  List IntersectionPoint ->
  Maybe (Sign, NonEmpty (Interval Unitless, Interval Unitless))
findOverlappingSegments [] = Nothing
findOverlappingSegments (NonEmpty endpointSolutions) = do
  let alignmentAt endpointSolution = case IntersectionPoint.kind endpointSolution of
        Tangent alignment -> Just alignment
        Crossing -> Nothing
  endpointSolutionAlignments <- Maybe.collect alignmentAt endpointSolutions
  alignment <- NonEmpty.uniqueValue endpointSolutionAlignments
  let isContinuation (IntersectionPoint _ t1 t2) =
        case alignment of
          Positive -> (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0)
          Negative -> (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0)
  let candidateEndpoints =
        endpointSolutions
          & NonEmpty.filter (not . isContinuation)
          & List.sortBy IntersectionPoint.firstParameterValue
  let isOverlappingSegment (IntersectionPoint _ start1 _) (IntersectionPoint _ end1 _) = do
        let tValues1 = Interval.sampleValues (Interval start1 end1)
        let samplePoints1 = NonEmpty.map (Curve.point curve1) tValues1
        NonEmpty.all (intersects curve2) samplePoints1
  let overlappingSegment (IntersectionPoint _ start1 start2) (IntersectionPoint _ end1 end2) =
        (Interval start1 end1, Interval start2 end2)
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
