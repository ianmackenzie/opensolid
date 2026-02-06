module OpenSolid.Curve2D.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Segment qualified as Segment
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.EndpointIntersection (EndpointIntersection (EndpointIntersection))
import OpenSolid.Curve2D.EndpointIntersection qualified as EndpointIntersection
import OpenSolid.Curve2D.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2D.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2D.OverlappingSegment qualified as OverlappingSegment
import OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain qualified as Domain
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)
  deriving (Show)

data TangentSolutionTargetSpace

data Problem units space = Problem
  { curve1 :: Curve2D units space
  , curve2 :: Curve2D units space
  , tangent1 :: DirectionCurve2D space
  , tangent2 :: DirectionCurve2D space
  , endpointIntersections :: List EndpointIntersection
  , crossingSolutionTarget :: VectorSurfaceFunction2D units space
  , tangentSolutionTarget :: VectorSurfaceFunction2D (units ?*? units) TangentSolutionTargetSpace
  }

data Classification
  = InteriorInterior
  | InteriorEndpoint Domain.Size
  | EndpointEndpoint Domain.Size Sign

classify :: Interval Unitless -> Interval Unitless -> Fuzzy Classification
classify tBounds1 tBounds2 =
  case (Domain.classify tBounds1, Domain.classify tBounds2) of
    (Domain.Interior, Domain.Interior) -> Resolved InteriorInterior
    (Domain.Interior, Domain.Start size) -> Resolved (InteriorEndpoint size)
    (Domain.Interior, Domain.End size) -> Resolved (InteriorEndpoint size)
    (Domain.Start size, Domain.Interior) -> Resolved (InteriorEndpoint size)
    (Domain.End size, Domain.Interior) -> Resolved (InteriorEndpoint size)
    (Domain.Start size1, Domain.Start size2) -> Resolved (EndpointEndpoint (max size1 size2) Negative)
    (Domain.Start size1, Domain.End size2) -> Resolved (EndpointEndpoint (max size1 size2) Positive)
    (Domain.End size1, Domain.Start size2) -> Resolved (EndpointEndpoint (max size1 size2) Positive)
    (Domain.End size1, Domain.End size2) -> Resolved (EndpointEndpoint (max size1 size2) Negative)
    (Domain.Entire, _) -> Unresolved
    (_, Domain.Entire) -> Unresolved

isInterior :: Number -> Interval Unitless -> Bool
isInterior tValue tBounds = Interval.includes tValue (Domain.interior tBounds)

crossingIntersection ::
  Tolerance units =>
  Problem units space ->
  Interval Unitless ->
  Interval Unitless ->
  Sign ->
  Fuzzy (Maybe IntersectionPoint)
crossingIntersection problem tBounds1 tBounds2 sign = do
  let Problem{curve1, curve2, crossingSolutionTarget} = problem
  let uvPoint0 = Point2D (Interval.midpoint tBounds1) (Interval.midpoint tBounds2)
  let Point2D t1 t2 = VectorSurfaceFunction2D.newtonRaphson crossingSolutionTarget uvPoint0
  let pointsAreEqual = Curve2D.evaluate curve1 t1 ~= Curve2D.evaluate curve2 t2
  if isInterior t1 tBounds1 && isInterior t2 tBounds2 && pointsAreEqual
    then Resolved (Just (IntersectionPoint.crossing t1 t2 sign))
    else Unresolved

tangentIntersection ::
  Tolerance units =>
  Problem units space ->
  Interval Unitless ->
  Interval Unitless ->
  Fuzzy (Maybe IntersectionPoint)
tangentIntersection problem tBounds1 tBounds2 = do
  let Problem{curve1, curve2, tangent1, tangent2, tangentSolutionTarget} = problem
  let uvPoint0 = Point2D (Interval.midpoint tBounds1) (Interval.midpoint tBounds2)
  let Point2D t1 t2 = VectorSurfaceFunction2D.newtonRaphson tangentSolutionTarget uvPoint0
  let pointsAreEqual = Curve2D.evaluate curve1 t1 ~= Curve2D.evaluate curve2 t2
  let tangentDirection1 = DirectionCurve2D.evaluate tangent1 t1
  let tangentDirection2 = DirectionCurve2D.evaluate tangent2 t2
  let crossProduct = tangentDirection1 `cross` tangentDirection2
  let tangentsAreParallel = Tolerance.using 1e-9 (crossProduct ~= Quantity.zero)
  if isInterior t1 tBounds1 && isInterior t2 tBounds2 && pointsAreEqual && tangentsAreParallel
    then Resolved (Just (IntersectionPoint.tangent t1 t2))
    else Unresolved

findIntersectionPoint ::
  Tolerance units =>
  Problem units space ->
  (Interval Unitless, Interval Unitless) ->
  (Curve2D.Segment units space, Curve2D.Segment units space) ->
  Fuzzy (Maybe IntersectionPoint)
findIntersectionPoint problem (tBounds1, tBounds2) (segment1, segment2) = do
  if not (Segment.bounds segment1 `intersects` Segment.bounds segment2)
    then Resolved Nothing
    else do
      classification <- classify tBounds1 tBounds2
      let tangentBounds1 = Segment.tangentBounds segment1
      let tangentBounds2 = Segment.tangentBounds segment2
      let crossingSign = Interval.resolvedSign (tangentBounds1 `cross` tangentBounds2)
      let uniqueTangentSolution = secondDerivativesIndependent segment1 segment2
      let interiorIntersection
            | Resolved sign <- crossingSign = crossingIntersection problem tBounds1 tBounds2 sign
            | uniqueTangentSolution = tangentIntersection problem tBounds1 tBounds2
            | otherwise = Unresolved
      let endpointIntersection size allowedAlignment = do
            let isLocal = EndpointIntersection.isLocal tBounds1 tBounds2
            case List.filter isLocal problem.endpointIntersections of
              [] -> Unresolved
              List.TwoOrMore -> Unresolved
              List.One EndpointIntersection{intersectionPoint, isSingular, alignment} ->
                if isSingular && size == Domain.Small
                  then Resolved (Just intersectionPoint)
                  else case intersectionPoint.kind of
                    IntersectionPoint.Crossing _ ->
                      case crossingSign of
                        Resolved _ -> Resolved (Just intersectionPoint)
                        Unresolved -> Unresolved
                    IntersectionPoint.Tangent ->
                      if uniqueTangentSolution || allowedAlignment == Just alignment
                        then Resolved (Just intersectionPoint)
                        else Unresolved
      case classification of
        InteriorInterior -> interiorIntersection
        InteriorEndpoint size -> endpointIntersection size Nothing
        EndpointEndpoint size allowedSign -> endpointIntersection size (Just allowedSign)

secondDerivativesIndependent :: Curve2D.Segment units space -> Curve2D.Segment units space -> Bool
secondDerivativesIndependent segment1 segment2 = do
  let VectorBounds2D x'1 y'1 = Segment.firstDerivativeBounds segment1
  let VectorBounds2D x'2 y'2 = Segment.firstDerivativeBounds segment2
  let VectorBounds2D x''1 y''1 = Segment.secondDerivativeBounds segment1
  let VectorBounds2D x''2 y''2 = Segment.secondDerivativeBounds segment2
  Interval.isResolved (d2ydx2Bounds x'1 y'1 x''1 y''1 .-. d2ydx2Bounds x'2 y'2 x''2 y''2)
    || Interval.isResolved (d2ydx2Bounds y'1 x'1 y''1 x''1 .-. d2ydx2Bounds y'2 x'2 y''2 x''2)

d2ydx2Bounds ::
  Interval units ->
  Interval units ->
  Interval units ->
  Interval units ->
  Interval (Unitless ?/? units)
d2ydx2Bounds x' y' x'' y'' =
  Units.simplify ((y'' ?*? x' .-. y' ?*? x'') ?/? (x' ?*? x' ?*? x'))

candidateOverlappingSegment ::
  EndpointIntersection ->
  EndpointIntersection ->
  List OverlappingSegment
candidateOverlappingSegment first second = do
  let tBounds1 = Interval first.intersectionPoint.t1 second.intersectionPoint.t1
  let tBounds2 = Interval first.intersectionPoint.t2 second.intersectionPoint.t2
  let firstIsStart = case first.alignment of
        Positive -> first.intersectionPoint.t2 < 1
        Negative -> first.intersectionPoint.t2 > 0
  let secondIsEnd = case second.alignment of
        Positive -> second.intersectionPoint.t2 > 0
        Negative -> second.intersectionPoint.t2 < 1
  let isCandidate = firstIsStart && secondIsEnd && first.alignment == second.alignment
  [OverlappingSegment{tBounds1, tBounds2, alignment = first.alignment} | isCandidate]

overlappingSegments ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  List EndpointIntersection ->
  Maybe (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 endpointIntersections =
  case List.concat (List.successive candidateOverlappingSegment endpointIntersections) of
    [] -> Nothing
    NonEmpty candidateOverlappingSegments -> do
      let boundsWidth1 segment = Interval.width segment.tBounds1
      let testSegment = NonEmpty.maximumBy boundsWidth1 candidateOverlappingSegments
      let tSampleValues1 = Interval.sampleValues testSegment.tBounds1
      let samplePoints1 = NonEmpty.map (Curve2D.evaluate curve1) tSampleValues1
      if NonEmpty.allSatisfy (intersects curve2) samplePoints1
        then Just candidateOverlappingSegments
        else Nothing

intersections ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  Result Curve.IsPoint (Maybe Intersections)
intersections curve1 curve2
  | not (Curve2D.bounds curve1 `intersects` Curve2D.bounds curve2) = Ok Nothing
  | otherwise = do
      searchTree1 <- Curve2D.searchTree curve1
      searchTree2 <- Curve2D.searchTree curve2
      let searchTree = Search.pairwise (,) searchTree1 searchTree2
      tangent1 <- Curve2D.tangentDirection curve1
      tangent2 <- Curve2D.tangentDirection curve2
      let endpointIntersections = EndpointIntersection.find searchTree1 searchTree2
      case overlappingSegments curve1 curve2 endpointIntersections of
        Just segments -> Ok (Just (OverlappingSegments segments))
        Nothing -> do
          let differenceSurface = curve2 `compose` V .-. curve1 `compose` U
          let derivativeSurface1 = Curve2D.derivative curve1 `compose` U
          let derivativeSurface2 = Curve2D.derivative curve2 `compose` V
          let tangentSolutionX = differenceSurface `dot_` derivativeSurface1
          let tangentSolutionY = derivativeSurface2 `cross_` derivativeSurface1
          let tangentSolutionTarget = VectorSurfaceFunction2D.xy tangentSolutionX tangentSolutionY
          let problem =
                Problem
                  { curve1
                  , curve2
                  , tangent1
                  , tangent2
                  , endpointIntersections
                  , crossingSolutionTarget = differenceSurface
                  , tangentSolutionTarget
                  }
          let solutions = Search.exclusive (findIntersectionPoint problem) searchTree
          case deduplicate solutions [] & List.map Pair.second & List.sort of
            [] -> Ok Nothing
            NonEmpty intersectionPoints -> Ok (Just (IntersectionPoints intersectionPoints))

type Solution = ((Interval Unitless, Interval Unitless), IntersectionPoint)

deduplicate :: List Solution -> List Solution -> List Solution
deduplicate [] accumulated = accumulated
deduplicate (first : rest) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicate rest accumulated
    else deduplicate rest (first : accumulated)

isDuplicate :: Solution -> Solution -> Bool
isDuplicate (uvBounds1, _) (uvBounds2, _) = Domain.touching uvBounds1 uvBounds2
