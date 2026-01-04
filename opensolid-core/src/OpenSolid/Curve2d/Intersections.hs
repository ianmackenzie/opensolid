module OpenSolid.Curve2d.Intersections
  ( Error (..)
  , Intersections (..)
  , intersections
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2d.OverlappingSegment qualified as OverlappingSegment
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Polymorphic.Point2d (Point2d (Point2d))
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

data Error
  = FirstCurveIsPoint
  | SecondCurveIsPoint
  | BothCurvesArePoints
  deriving (Eq, Show)

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)
  deriving (Show)

data TangentSolutionTargetSpace

data Problem units space = Problem
  { curve1 :: Curve2d units space
  , curve2 :: Curve2d units space
  , endpointIntersections :: List EndpointIntersection
  , crossingSolutionTarget :: VectorSurfaceFunction2d units space
  , tangentSolutionTarget :: VectorSurfaceFunction2d (units ?*? units) TangentSolutionTargetSpace
  }

data EndpointIntersection = EndpointIntersection
  { intersectionPoint :: IntersectionPoint
  , isSingular :: Bool
  , alignment :: Sign
  }
  deriving (Show)

findEndpointsOf ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result Curve2d.IsPoint (List Number, List Number)
findEndpointsOf curve1 curve2 = do
  let (start1, end1) = Curve2d.endpoints curve1
  Result.map2 (,) (Curve2d.findPoint start1 curve2) (Curve2d.findPoint end1 curve2)

findEndpointIntersections ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result Error (List EndpointIntersection)
findEndpointIntersections curve1 curve2 =
  case (findEndpointsOf curve1 curve2, findEndpointsOf curve2 curve1) of
    (Ok (start1t2s, end1t2s), Ok (start2t1s, end2t1s)) -> Ok do
      List.concat
        [ List.map (0,) start1t2s
        , List.map (1,) end1t2s
        , List.map (,0) start2t1s
        , List.map (,1) end2t1s
        ]
        & List.sortAndDeduplicate
        & List.map (toEndpointIntersection curve1 curve2)
    (Error Curve2d.IsPoint, Ok{}) -> Error FirstCurveIsPoint
    (Ok{}, Error Curve2d.IsPoint) -> Error SecondCurveIsPoint
    (Error Curve2d.IsPoint, Error Curve2d.IsPoint) -> Error BothCurvesArePoints

toEndpointIntersection ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  (Number, Number) ->
  EndpointIntersection
toEndpointIntersection curve1 curve2 (t1, t2) = do
  let (tangentVector1, scale1, singular1) = tangentSignature curve1 t1
  let (tangentVector2, scale2, singular2) = tangentSignature curve2 t2
  let crossProduct = tangentVector1 `cross` tangentVector2
  let scale = max scale1 scale2
  let intersectionPoint =
        if scale .*. crossProduct ~= Quantity.zero
          then IntersectionPoint.tangent t1 t2
          else IntersectionPoint.crossing t1 t2 (Quantity.sign crossProduct)
  let isSingular = singular1 || singular2
  let alignment = Quantity.sign (tangentVector1 `dot` tangentVector2)
  EndpointIntersection{intersectionPoint, isSingular, alignment}

tangentSignature ::
  Tolerance units =>
  Curve2d units space ->
  Number ->
  (Vector2d Unitless space, Quantity units, Bool)
tangentSignature curve tValue = do
  let firstDerivative = VectorCurve2d.evaluate curve.derivative tValue
  let secondDerivative = VectorCurve2d.evaluate curve.derivative.derivative tValue
  let isSingular = firstDerivative ~= Vector2d.zero
  let discriminator
        | isSingular && tValue == 0 = 0.5 *. secondDerivative
        | isSingular && tValue == 1 = -0.5 *. secondDerivative
        | otherwise = firstDerivative
  let scale = Vector2d.magnitude discriminator
  let tangentVector = discriminator ./. scale
  (tangentVector, scale, isSingular)

findIntersectionPoint ::
  Tolerance units =>
  Problem units space ->
  (Bounds Unitless, Bounds Unitless) ->
  Fuzzy (Maybe IntersectionPoint)
findIntersectionPoint problem (tBounds1, tBounds2) = do
  let Problem{curve1, curve2} = problem
  let interiorBounds1 = Curve2d.evaluateBounds curve1 (Bisection.interior tBounds1)
  let interiorBounds2 = Curve2d.evaluateBounds curve2 (Bisection.interior tBounds2)
  if not (interiorBounds1 `intersects` interiorBounds2)
    then Resolved Nothing
    else do
      let subdomain1 = Bisection.classify tBounds1
      let subdomain2 = Bisection.classify tBounds2
      let firstDerivative1 = Curve2d.derivative curve1
      let firstDerivative2 = Curve2d.derivative curve2
      let secondDerivative1 = VectorCurve2d.derivative firstDerivative1
      let secondDerivative2 = VectorCurve2d.derivative firstDerivative2
      let firstBounds1 = VectorCurve2d.evaluateBounds firstDerivative1 tBounds1
      let firstBounds2 = VectorCurve2d.evaluateBounds firstDerivative2 tBounds2
      let secondBounds1 = VectorCurve2d.evaluateBounds secondDerivative1 tBounds1
      let secondBounds2 = VectorCurve2d.evaluateBounds secondDerivative2 tBounds2
      let firstCrossProductSign = Bounds.resolvedSign (firstBounds1 `cross_` firstBounds2)
      let uniqueTangentSolution =
            secondDerivativesIndependent firstBounds1 firstBounds2 secondBounds1 secondBounds2
      let isInterior t1 t2 = Bisection.isInterior t1 tBounds1 && Bisection.isInterior t2 tBounds2
      let equalPoints t1 t2 = Curve2d.evaluate curve1 t1 ~= Curve2d.evaluate curve2 t2
      let equalTangents t1 t2 = do
            let tangent1 = Vector2d.normalize (VectorCurve2d.evaluate firstDerivative1 t1)
            let tangent2 = Vector2d.normalize (VectorCurve2d.evaluate firstDerivative2 t2)
            Tolerance.using 1e-9 (tangent1 `cross` tangent2 ~= Quantity.zero)
      let resolvedEndpointIntersection size allowedAlignment = do
            let isLocal EndpointIntersection{intersectionPoint} =
                  Bounds.includes intersectionPoint.t1 tBounds1
                    && Bounds.includes intersectionPoint.t2 tBounds2
            case List.filter isLocal problem.endpointIntersections of
              [] -> Unresolved
              List.One (EndpointIntersection{intersectionPoint, isSingular, alignment}) ->
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
              List.TwoOrMore -> Unresolved
      case (subdomain1, subdomain2) of
        (Bisection.Interior, Bisection.Interior)
          | Resolved sign <- firstCrossProductSign -> do
              let uvPoint0 = Point2d (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              let Point2d t1 t2 = NewtonRaphson.surface2d problem.crossingSolutionTarget uvPoint0
              if isInterior t1 t2 && equalPoints t1 t2
                then Resolved (Just (IntersectionPoint.crossing t1 t2 sign))
                else Unresolved
          | uniqueTangentSolution -> do
              let uvPoint0 = Point2d (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              let Point2d t1 t2 = NewtonRaphson.surface2d problem.tangentSolutionTarget uvPoint0
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
  VectorBounds2d units space ->
  VectorBounds2d units space ->
  VectorBounds2d units space ->
  VectorBounds2d units space ->
  Bool
secondDerivativesIndependent firstBounds1 firstBounds2 secondBounds1 secondBounds2 = do
  let VectorBounds2d x'1 y'1 = firstBounds1
  let VectorBounds2d x'2 y'2 = firstBounds2
  let VectorBounds2d x''1 y''1 = secondBounds1
  let VectorBounds2d x''2 y''2 = secondBounds2
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
  (EndpointIntersection, EndpointIntersection) ->
  List OverlappingSegment
candidateOverlappingSegment (first, second) = do
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
  Curve2d units space ->
  Curve2d units space ->
  List EndpointIntersection ->
  Maybe (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 endpointIntersections =
  case List.combine candidateOverlappingSegment (List.successive (,) endpointIntersections) of
    [] -> Nothing
    NonEmpty candidateOverlappingSegments -> do
      let boundsWidth1 segment = Bounds.width segment.tBounds1
      let testSegment = NonEmpty.maximumBy boundsWidth1 candidateOverlappingSegments
      let tSampleValues1 = Bounds.sampleValues testSegment.tBounds1
      let samplePoints1 = List.map (Curve2d.evaluate curve1) tSampleValues1
      if List.allSatisfy (intersects curve2) samplePoints1
        then Just candidateOverlappingSegments
        else Nothing

intersections ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result Error (Maybe Intersections)
intersections curve1 curve2
  | not (Curve2d.bounds curve1 `intersects` Curve2d.bounds curve2) = Ok Nothing
  | otherwise = do
      endpointIntersections <- findEndpointIntersections curve1 curve2
      case overlappingSegments curve1 curve2 endpointIntersections of
        Just segments -> Ok (Just (OverlappingSegments segments))
        Nothing -> do
          let curve1Surface = curve1 `compose` U
          let curve2Surface = curve2 `compose` V
          let derivative1Surface = Curve2d.derivative curve1 `compose` U
          let derivative2Surface = Curve2d.derivative curve2 `compose` V
          let differenceSurface = curve2Surface .-. curve1Surface
          let problem =
                Problem
                  { curve1
                  , curve2
                  , endpointIntersections
                  , crossingSolutionTarget = differenceSurface
                  , tangentSolutionTarget =
                      VectorSurfaceFunction2d.xy
                        (differenceSurface `dot_` derivative1Surface)
                        (derivative2Surface `cross_` derivative1Surface)
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
isDuplicate ((uBounds1, vBounds1), _) ((uBounds2, vBounds2), _) =
  Bounds.overlap uBounds1 uBounds2 >= 0 && Bounds.overlap vBounds1 vBounds2 >= 0
