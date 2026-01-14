module OpenSolid.Curve2D.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Bisection qualified as Bisection
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
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
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
  | InteriorEndpoint Bisection.Size
  | EndpointEndpoint Bisection.Size Sign

classify :: Interval Unitless -> Interval Unitless -> Fuzzy Classification
classify tBounds1 tBounds2 =
  case (Bisection.classify tBounds1, Bisection.classify tBounds2) of
    (Bisection.Interior, Bisection.Interior) -> Resolved InteriorInterior
    (Bisection.Interior, Bisection.Start size) -> Resolved (InteriorEndpoint size)
    (Bisection.Interior, Bisection.End size) -> Resolved (InteriorEndpoint size)
    (Bisection.Start size, Bisection.Interior) -> Resolved (InteriorEndpoint size)
    (Bisection.End size, Bisection.Interior) -> Resolved (InteriorEndpoint size)
    (Bisection.Start size1, Bisection.Start size2) -> Resolved (EndpointEndpoint (max size1 size2) Negative)
    (Bisection.Start size1, Bisection.End size2) -> Resolved (EndpointEndpoint (max size1 size2) Positive)
    (Bisection.End size1, Bisection.Start size2) -> Resolved (EndpointEndpoint (max size1 size2) Positive)
    (Bisection.End size1, Bisection.End size2) -> Resolved (EndpointEndpoint (max size1 size2) Negative)
    (Bisection.Entire, _) -> Unresolved
    (_, Bisection.Entire) -> Unresolved

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
  let Point2D t1 t2 = NewtonRaphson.surface2D crossingSolutionTarget uvPoint0
  let isInterior = Bisection.isInterior t1 tBounds1 && Bisection.isInterior t2 tBounds2
  let pointsAreEqual = Curve2D.evaluate curve1 t1 ~= Curve2D.evaluate curve2 t2
  if isInterior && pointsAreEqual
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
  let Point2D t1 t2 = NewtonRaphson.surface2D tangentSolutionTarget uvPoint0
  let isInterior = Bisection.isInterior t1 tBounds1 && Bisection.isInterior t2 tBounds2
  let pointsAreEqual = Curve2D.evaluate curve1 t1 ~= Curve2D.evaluate curve2 t2
  let tangentDirection1 = DirectionCurve2D.evaluate tangent1 t1
  let tangentDirection2 = DirectionCurve2D.evaluate tangent2 t2
  let crossProduct = tangentDirection1 `cross` tangentDirection2
  let tangentsAreParallel = Tolerance.using 1e-9 (crossProduct ~= Quantity.zero)
  if isInterior && pointsAreEqual && tangentsAreParallel
    then Resolved (Just (IntersectionPoint.tangent t1 t2))
    else Unresolved

findIntersectionPoint ::
  Tolerance units =>
  Problem units space ->
  (Interval Unitless, Interval Unitless) ->
  Fuzzy (Maybe IntersectionPoint)
findIntersectionPoint problem (tBounds1, tBounds2) = do
  let Problem{curve1, curve2, tangent1, tangent2} = problem
  let interiorBounds1 = Curve2D.evaluateBounds curve1 (Bisection.interior tBounds1)
  let interiorBounds2 = Curve2D.evaluateBounds curve2 (Bisection.interior tBounds2)
  if not (interiorBounds1 `intersects` interiorBounds2)
    then Resolved Nothing
    else do
      classification <- classify tBounds1 tBounds2
      let tangentBounds1 = DirectionCurve2D.evaluateBounds tangent1 tBounds1
      let tangentBounds2 = DirectionCurve2D.evaluateBounds tangent2 tBounds2
      let crossingSign = Interval.resolvedSign (tangentBounds1 `cross` tangentBounds2)
      let uniqueTangentSolution = secondDerivativesIndependent curve1 curve2 tBounds1 tBounds2
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
                if isSingular && size == Bisection.Small
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

secondDerivativesIndependent ::
  Curve2D units space ->
  Curve2D units space ->
  Interval Unitless ->
  Interval Unitless ->
  Bool
secondDerivativesIndependent curve1 curve2 tBounds1 tBounds2 = do
  let firstDerivative1 = Curve2D.derivative curve1
  let firstDerivative2 = Curve2D.derivative curve2
  let secondDerivative1 = VectorCurve2D.derivative firstDerivative1
  let secondDerivative2 = VectorCurve2D.derivative firstDerivative2
  let VectorBounds2D x'1 y'1 = VectorCurve2D.evaluateBounds firstDerivative1 tBounds1
  let VectorBounds2D x'2 y'2 = VectorCurve2D.evaluateBounds firstDerivative2 tBounds2
  let VectorBounds2D x''1 y''1 = VectorCurve2D.evaluateBounds secondDerivative1 tBounds1
  let VectorBounds2D x''2 y''2 = VectorCurve2D.evaluateBounds secondDerivative2 tBounds2
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
  Result Curve2D.IsPoint (Maybe Intersections)
intersections curve1 curve2
  | not (Curve2D.bounds curve1 `intersects` Curve2D.bounds curve2) = Ok Nothing
  | otherwise = do
      tangent1 <- Curve2D.tangentDirection curve1
      tangent2 <- Curve2D.tangentDirection curve2
      endpointIntersections <- EndpointIntersection.find curve1 curve2 tangent1 tangent2
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
          case Bisection.search Bisection.curvePairDomain (findIntersectionPoint problem) of
            Error Bisection.InfiniteRecursion -> throw HigherOrderZero
            Ok solutions -> case deduplicate solutions [] & List.map Pair.second & List.sort of
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
isDuplicate (uvBounds1, _) (uvBounds2, _) = Bisection.overlaps uvBounds1 uvBounds2
