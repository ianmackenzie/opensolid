module OpenSolid.Curve2D.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2D.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2D.OverlappingSegment qualified as OverlappingSegment
import OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
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
import OpenSolid.Vector2D qualified as Vector2D
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

data EndpointIntersection = EndpointIntersection
  { intersectionPoint :: IntersectionPoint
  , isSingular :: Bool
  , alignment :: Sign
  }
  deriving (Show)

findEndpointIntersections ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  DirectionCurve2D space ->
  DirectionCurve2D space ->
  Result Curve2D.IsPoint (List EndpointIntersection)
findEndpointIntersections curve1 curve2 tangent1 tangent2 = do
  start1t2s <- Curve2D.findPoint (Curve2D.startPoint curve1) curve2
  end1t2s <- Curve2D.findPoint (Curve2D.endPoint curve1) curve2
  start2t1s <- Curve2D.findPoint (Curve2D.startPoint curve2) curve1
  end2t1s <- Curve2D.findPoint (Curve2D.endPoint curve2) curve1
  let start1Solutions = List.map (0,) start1t2s
  let end1Solutions = List.map (1,) end1t2s
  let start2Solutions = List.map (,0) start2t1s
  let end2Solutions = List.map (,1) end2t1s
  let allSolutions = List.concat [start1Solutions, end1Solutions, start2Solutions, end2Solutions]
  let uniqueSolutions = List.sortAndDeduplicate allSolutions
  Ok (List.map (toEndpointIntersection curve1 curve2 tangent1 tangent2) uniqueSolutions)

toEndpointIntersection ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  DirectionCurve2D space ->
  DirectionCurve2D space ->
  (Number, Number) ->
  EndpointIntersection
toEndpointIntersection curve1 curve2 tangent1 tangent2 (t1, t2) = do
  let tangentDirection1 = DirectionCurve2D.evaluate tangent1 t1
  let tangentDirection2 = DirectionCurve2D.evaluate tangent2 t2
  let crossProduct = tangentDirection1 `cross` tangentDirection2
  let singular1 = VectorCurve2D.evaluate (Curve2D.derivative curve1) t1 ~= Vector2D.zero
  let singular2 = VectorCurve2D.evaluate (Curve2D.derivative curve2) t2 ~= Vector2D.zero
  let intersectionPoint =
        if Tolerance.using 1e-9 (crossProduct ~= Quantity.zero)
          then IntersectionPoint.tangent t1 t2
          else IntersectionPoint.crossing t1 t2 (Quantity.sign crossProduct)
  let isSingular = singular1 || singular2
  let alignment = Quantity.sign (tangentDirection1 `dot` tangentDirection2)
  EndpointIntersection{intersectionPoint, isSingular, alignment}

findIntersectionPoint ::
  Tolerance units =>
  Problem units space ->
  (Bounds Unitless, Bounds Unitless) ->
  Fuzzy (Maybe IntersectionPoint)
findIntersectionPoint problem (tBounds1, tBounds2) = do
  let Problem{curve1, curve2, tangent1, tangent2} = problem
  let interiorBounds1 = Curve2D.evaluateBounds curve1 (Bisection.interior tBounds1)
  let interiorBounds2 = Curve2D.evaluateBounds curve2 (Bisection.interior tBounds2)
  if not (interiorBounds1 `intersects` interiorBounds2)
    then Resolved Nothing
    else do
      let firstDerivative1 = Curve2D.derivative curve1
      let firstDerivative2 = Curve2D.derivative curve2
      let secondDerivative1 = VectorCurve2D.derivative firstDerivative1
      let secondDerivative2 = VectorCurve2D.derivative firstDerivative2
      let firstBounds1 = VectorCurve2D.evaluateBounds firstDerivative1 tBounds1
      let firstBounds2 = VectorCurve2D.evaluateBounds firstDerivative2 tBounds2
      let secondBounds1 = VectorCurve2D.evaluateBounds secondDerivative1 tBounds1
      let secondBounds2 = VectorCurve2D.evaluateBounds secondDerivative2 tBounds2
      let firstCrossProductSign = Bounds.resolvedSign (firstBounds1 `cross_` firstBounds2)
      let uniqueTangentSolution =
            secondDerivativesIndependent firstBounds1 firstBounds2 secondBounds1 secondBounds2
      let isInterior t1 t2 = Bisection.isInterior t1 tBounds1 && Bisection.isInterior t2 tBounds2
      let equalPoints t1 t2 = Curve2D.evaluate curve1 t1 ~= Curve2D.evaluate curve2 t2
      let equalTangents t1 t2 = do
            let tangentDirection1 = DirectionCurve2D.evaluate tangent1 t1
            let tangentDirection2 = DirectionCurve2D.evaluate tangent2 t2
            Tolerance.using 1e-9 (tangentDirection1 `cross` tangentDirection2 ~= Quantity.zero)
      let resolvedEndpointIntersection size allowedAlignment = do
            let isLocal EndpointIntersection{intersectionPoint} =
                  Bounds.includes intersectionPoint.t1 tBounds1
                    && Bounds.includes intersectionPoint.t2 tBounds2
            case List.filter isLocal problem.endpointIntersections of
              [] -> Unresolved
              List.TwoOrMore -> Unresolved
              List.One EndpointIntersection{intersectionPoint, isSingular, alignment} ->
                if isSingular && size == Bisection.Small
                  then Resolved (Just intersectionPoint)
                  else case intersectionPoint.kind of
                    IntersectionPoint.Crossing _ ->
                      case firstCrossProductSign of
                        Resolved _ -> Resolved (Just intersectionPoint)
                        Unresolved -> Unresolved
                    IntersectionPoint.Tangent ->
                      if uniqueTangentSolution || allowedAlignment == Just alignment
                        then Resolved (Just intersectionPoint)
                        else Unresolved
      case (Bisection.classify tBounds1, Bisection.classify tBounds2) of
        (Bisection.Interior, Bisection.Interior)
          | Resolved sign <- firstCrossProductSign -> do
              let uvPoint0 = Point2D (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              let Point2D t1 t2 = NewtonRaphson.surface2D problem.crossingSolutionTarget uvPoint0
              if isInterior t1 t2 && equalPoints t1 t2
                then Resolved (Just (IntersectionPoint.crossing t1 t2 sign))
                else Unresolved
          | uniqueTangentSolution -> do
              let uvPoint0 = Point2D (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              let Point2D t1 t2 = NewtonRaphson.surface2D problem.tangentSolutionTarget uvPoint0
              if isInterior t1 t2 && equalPoints t1 t2 && equalTangents t1 t2
                then Resolved (Just (IntersectionPoint.tangent t1 t2))
                else Unresolved
          | otherwise -> Unresolved
        (Bisection.Interior, Bisection.Start size) -> resolvedEndpointIntersection size Nothing
        (Bisection.Interior, Bisection.End size) -> resolvedEndpointIntersection size Nothing
        (Bisection.Start size, Bisection.Interior) -> resolvedEndpointIntersection size Nothing
        (Bisection.End size, Bisection.Interior) -> resolvedEndpointIntersection size Nothing
        (Bisection.Start size1, Bisection.Start size2) -> resolvedEndpointIntersection (max size1 size2) (Just Negative)
        (Bisection.Start size1, Bisection.End size2) -> resolvedEndpointIntersection (max size1 size2) (Just Positive)
        (Bisection.End size1, Bisection.Start size2) -> resolvedEndpointIntersection (max size1 size2) (Just Positive)
        (Bisection.End size1, Bisection.End size2) -> resolvedEndpointIntersection (max size1 size2) (Just Negative)
        (Bisection.Entire, _) -> Unresolved
        (_, Bisection.Entire) -> Unresolved

secondDerivativesIndependent ::
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  Bool
secondDerivativesIndependent firstBounds1 firstBounds2 secondBounds1 secondBounds2 = do
  let VectorBounds2D x'1 y'1 = firstBounds1
  let VectorBounds2D x'2 y'2 = firstBounds2
  let VectorBounds2D x''1 y''1 = secondBounds1
  let VectorBounds2D x''2 y''2 = secondBounds2
  Bounds.isResolved (d2ydx2Bounds x'1 y'1 x''1 y''1 .-. d2ydx2Bounds x'2 y'2 x''2 y''2)
    || Bounds.isResolved (d2ydx2Bounds y'1 x'1 y''1 x''1 .-. d2ydx2Bounds y'2 x'2 y''2 x''2)

d2ydx2Bounds ::
  Bounds units ->
  Bounds units ->
  Bounds units ->
  Bounds units ->
  Bounds (Unitless ?/? units)
d2ydx2Bounds x' y' x'' y'' =
  Units.simplify ((y'' ?*? x' .-. y' ?*? x'') ?/? (x' ?*? x' ?*? x'))

candidateOverlappingSegment ::
  EndpointIntersection ->
  EndpointIntersection ->
  List OverlappingSegment
candidateOverlappingSegment first second = do
  let tBounds1 = Bounds first.intersectionPoint.t1 second.intersectionPoint.t1
  let tBounds2 = Bounds first.intersectionPoint.t2 second.intersectionPoint.t2
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
      let boundsWidth1 segment = Bounds.width segment.tBounds1
      let testSegment = NonEmpty.maximumBy boundsWidth1 candidateOverlappingSegments
      let tSampleValues1 = Bounds.sampleValues testSegment.tBounds1
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
      endpointIntersections <- findEndpointIntersections curve1 curve2 tangent1 tangent2
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

type Solution = ((Bounds Unitless, Bounds Unitless), IntersectionPoint)

deduplicate :: List Solution -> List Solution -> List Solution
deduplicate [] accumulated = accumulated
deduplicate (first : rest) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicate rest accumulated
    else deduplicate rest (first : accumulated)

isDuplicate :: Solution -> Solution -> Bool
isDuplicate (uvBounds1, _) (uvBounds2, _) = Bisection.overlaps uvBounds1 uvBounds2
