{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint, Kind (Crossing, Tangent))
import OpenSolid.Curve.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (IsDegenerate, Nondegenerate)
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain qualified as Search.Domain
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve qualified as VectorCurve

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments Sign (NonEmpty (Interval Unitless, Interval Unitless))
  deriving (Show)

data EndpointSolution dimension space = EndpointSolution
  { t1 :: Number
  , t2 :: Number
  , kind :: IntersectionPoint.Kind
  , isDegenerate :: ~Bool
  }
  deriving (Show)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe Intersections)
intersections curve1 curve2 = do
  nondegenerate1 <- Curve.nondegenerate curve1
  nondegenerate2 <- Curve.nondegenerate curve2
  if not (Curve.overallBounds curve1 `intersects` Curve.overallBounds curve2)
    then Ok Nothing
    else do
      let endpointSolutions = findEndpointSolutions nondegenerate1 nondegenerate2
      Ok $ case findOverlappingSegments curve1 curve2 endpointSolutions of
        Just (alignment, segments) -> Just (OverlappingSegments alignment segments)
        Nothing -> do
          let toIntersectionPoint EndpointSolution{kind, t1, t2} = IntersectionPoint.new kind t1 t2
          let endpointIntersectionPoints = List.map toIntersectionPoint endpointSolutions
          let interiorIntersectionPoints =
                findInteriorIntersectionPoints
                  (Nondegenerate.interior nondegenerate1)
                  (Nondegenerate.interior nondegenerate2)
                  endpointSolutions
          case List.sort (endpointIntersectionPoints <> interiorIntersectionPoints) of
            NonEmpty intersectionPoints -> Just (IntersectionPoints intersectionPoints)
            [] -> Nothing

findEndpointSolutions ::
  ( Curve.Exists dimension units space
  , Tolerance units
  , VectorCurve.Exists dimension units space
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  List (EndpointSolution dimension space)
findEndpointSolutions nondegenerate1 nondegenerate2 = do
  let curve1 = Nondegenerate.unwrap nondegenerate1
  let curve2 = Nondegenerate.unwrap nondegenerate2
  let findPoint curve t searchCurve = Curve.findPoint (Curve.point curve t) searchCurve
  let endpoints1On2 = [(t1, t2) | t1 <- [0.0, 1.0], t2 <- findPoint curve1 t1 curve2]
  let endpoints2On1 = [(t1, t2) | t2 <- [0.0, 1.0], t1 <- findPoint curve2 t2 curve1]
  let parameterValues = List.uniqueValues (endpoints1On2 <> endpoints2On1)
  let endpointSolution (t1, t2) = do
        let tangentDirection1 = Curve.Nondegenerate.tangentDirectionValue nondegenerate1 t1
        let tangentDirection2 = Curve.Nondegenerate.tangentDirectionValue nondegenerate2 t2
        let kind
              | tangentDirection1 ~= tangentDirection2 = Tangent Positive
              | tangentDirection1 ~= -tangentDirection2 = Tangent Negative
              | otherwise = Crossing
        let isDegenerate =
              Curve.derivativeValue curve1 t1 ~= Vector.zero
                || Curve.derivativeValue curve2 t2 ~= Vector.zero
        EndpointSolution{t1, t2, kind, isDegenerate}
  List.map endpointSolution parameterValues

findInteriorSolution ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface solveDimension solveUnits solveSpace
  , Tolerance units
  ) =>
  NewtonRaphson.EvaluateSurface solveDimension solveUnits solveSpace ->
  Curve dimension units space ->
  Curve dimension units space ->
  Interval Unitless ->
  Interval Unitless ->
  Maybe (Number, Number)
findInteriorSolution evaluate curve1 curve2 tBounds1 tBounds2 = do
  let uvPoint0 = UvPoint (Interval.midpoint tBounds1) (Interval.midpoint tBounds2)
  let UvPoint t1 t2 = NewtonRaphson.surface evaluate uvPoint0
  let isInterior1 = Search.isInterior t1 tBounds1
  let isInterior2 = Search.isInterior t2 tBounds2
  let pointsAreEqual = Curve.point curve1 t1 ~= Curve.point curve2 t2
  if isInterior1 && isInterior2 && pointsAreEqual then Just (t1, t2) else Nothing

findInteriorIntersectionPoints ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Nonzero (Curve dimension units space) ->
  Nonzero (Curve dimension units space) ->
  List (EndpointSolution dimension space) ->
  List IntersectionPoint
findInteriorIntersectionPoints nonzero1 nonzero2 endpointSolutions = do
  let nondegenerate1 = Nondegenerate.exterior nonzero1
  let nondegenerate2 = Nondegenerate.exterior nonzero2
  let curve1 = Nondegenerate.unwrap nondegenerate1
  let curve2 = Nondegenerate.unwrap nondegenerate2
  let searchTree = Search.pairwise (,) (Curve.searchTree curve1) (Curve.searchTree curve2)
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
  let interiorIntersectionPoint (tBounds1, tBounds2) (segment1, segment2)
        | not (Curve.Segment.bounds segment1 `intersects` Curve.Segment.bounds segment2) =
            Resolved Nothing
        | otherwise = do
            let tangentBounds1 = Curve.Segment.tangentBounds segment1
            let tangentBounds2 = Curve.Segment.tangentBounds segment2
            let curvatureVectorBounds1_ = Curve.Segment.curvatureVectorBounds_ segment1
            let curvatureVectorBounds2_ = Curve.Segment.curvatureVectorBounds_ segment2
            let isCrossing = DirectionBounds.areIndependent tangentBounds1 tangentBounds2
            let curvatureVectorsAreDistinct =
                  VectorBounds.areDistinct curvatureVectorBounds1_ curvatureVectorBounds2_
            let isLocal EndpointSolution{t1, t2} =
                  Interval.includes t1 tBounds1 && Interval.includes t2 tBounds2
            let singleEndpointSolution =
                  case List.filter isLocal endpointSolutions of
                    List.One endpointSolution -> Just endpointSolution
                    [] -> Nothing
                    List.TwoOrMore -> Nothing
            let hasCrossingEndpointSolution = case singleEndpointSolution of
                  Just EndpointSolution{kind} -> case kind of
                    Crossing -> True
                    Tangent _ -> False
                  Nothing -> False
            let hasContinuation = case singleEndpointSolution of
                  Just EndpointSolution{t1, t2}
                    | (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0) ->
                        DirectionBounds.areDistinct tangentBounds1 tangentBounds2
                    | (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0) ->
                        DirectionBounds.areDistinct tangentBounds1 -tangentBounds2
                    | otherwise -> False
                  Nothing -> False
            let hasTangentEndpointSolution = case singleEndpointSolution of
                  Just EndpointSolution{kind} -> case kind of
                    Tangent _ -> True
                    Crossing -> False
                  Nothing -> False
            let hasDegenerateEndpointSolution = case singleEndpointSolution of
                  Just EndpointSolution{isDegenerate} -> isDegenerate
                  Nothing -> False
            let isSmall = Search.Domain.isSmall tBounds1
            if
              | hasContinuation -> Resolved Nothing
              | isCrossing && hasCrossingEndpointSolution -> Resolved Nothing
              | curvatureVectorsAreDistinct && hasTangentEndpointSolution -> Resolved Nothing
              | isSmall && hasDegenerateEndpointSolution -> Resolved Nothing
              | isCrossing ->
                  case findInteriorSolution evaluateCrossing curve1 curve2 tBounds1 tBounds2 of
                    Just (t1, t2) -> Resolved (Just (IntersectionPoint.crossing t1 t2))
                    Nothing -> Unresolved
              | curvatureVectorsAreDistinct ->
                  case findInteriorSolution evaluateTangent curve1 curve2 tBounds1 tBounds2 of
                    Just (t1, t2) -> do
                      let tangentVector1 = Curve.Nonzero.tangentDirectionValue nonzero1 t1
                      let tangentVector2 = Curve.Nonzero.tangentDirectionValue nonzero2 t2
                      Tolerance.using Tolerance.unitless do
                        if
                          | tangentVector1 ~= tangentVector2 ->
                              Resolved (Just (IntersectionPoint.tangent t1 t2 Positive))
                          | tangentVector1 ~= -tangentVector2 ->
                              Resolved (Just (IntersectionPoint.tangent t1 t2 Negative))
                          | otherwise -> Unresolved
                    Nothing -> Unresolved
              | otherwise -> Unresolved
  let isDuplicate (tBounds1, _) (tBounds2, _) = Search.Domain.overlapping tBounds1 tBounds2
  List.map Pair.second (Search.exclusive interiorIntersectionPoint isDuplicate searchTree)

findOverlappingSegments ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Curve dimension units space ->
  List (EndpointSolution dimension space) ->
  Maybe (Sign, NonEmpty (Interval Unitless, Interval Unitless))
findOverlappingSegments _ _ [] = Nothing
findOverlappingSegments curve1 curve2 (NonEmpty endpointSolutions) = do
  let alignmentAt EndpointSolution{kind} = case kind of
        Tangent alignment -> Just alignment
        Crossing -> Nothing
  endpointSolutionAlignments <- Maybe.collect alignmentAt endpointSolutions
  alignment <- NonEmpty.uniqueValue endpointSolutionAlignments
  let isContinuation EndpointSolution{t1, t2} = case alignment of
        Positive -> (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0)
        Negative -> (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0)
  let candidateEndpoints =
        endpointSolutions
          & NonEmpty.filter (not . isContinuation)
          & List.sortBy (.t1)
  let isOverlappingSegment start end = do
        let tValues1 = Interval.sampleValues (Interval start.t1 end.t1)
        let samplePoints1 = NonEmpty.map (Curve.point curve1) tValues1
        let onCurve2 point = not (List.isEmpty (Curve.findPoint point curve2))
        NonEmpty.all onCurve2 samplePoints1
  let overlappingSegment start end = (Interval start.t1 end.t1, Interval start.t2 end.t2)
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
