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
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceParameter (SurfaceParameter (U))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import Prelude (max)

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

data Problem (coordinateSystem :: CoordinateSystem) where
  Problem ::
    { curve1 :: Curve2d (space @ units)
    , curve2 :: Curve2d (space @ units)
    , endpointIntersections :: List EndpointIntersection
    , crossingSolutionTarget :: VectorSurfaceFunction2d (space @ units)
    , tangentSolutionTarget :: VectorSurfaceFunction2d (TangentSolutionTargetSpace @ units)
    } ->
    Problem (space @ units)

data EndpointIntersection = EndpointIntersection
  { intersectionPoint :: IntersectionPoint
  , isSingular :: Bool
  , alignment :: Sign
  }
  deriving (Show)

data SubdomainClassification
  = Inner
  | Start SubdomainSize
  | End SubdomainSize
  deriving (Show)

data SubdomainSize = Small | Large deriving (Eq, Ord, Show)

classifySubdomain :: Bounds Unitless -> Fuzzy SubdomainClassification
classifySubdomain (Bounds tLow tHigh)
  | 0 < tLow && tHigh < 1 = Resolved Inner
  | tHigh < 1 = Resolved (Start (if tHigh <= Desingularization.t0 then Small else Large))
  | 0 < tLow = Resolved (End (if tLow >= Desingularization.t1 then Small else Large))
  | otherwise = Unresolved

findEndpointsOf ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Curve2d.IsPoint (List Number, List Number)
findEndpointsOf curve1 curve2 = do
  let (start1, end1) = Curve2d.endpoints curve1
  Result.map2 (,) (Curve2d.findPoint start1 curve2) (Curve2d.findPoint end1 curve2)

findEndpointIntersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
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
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  (Number, Number) ->
  EndpointIntersection
toEndpointIntersection curve1 curve2 (t1, t2) = do
  let (tangentVector1, scale1, singular1) = tangentSignature curve1 t1
  let (tangentVector2, scale2, singular2) = tangentSignature curve2 t2
  let crossProduct = tangentVector1 `cross` tangentVector2
  let scale = Quantity.max scale1 scale2
  let intersectionPoint =
        if scale .*. crossProduct ~= Quantity.zero
          then IntersectionPoint.tangent t1 t2
          else IntersectionPoint.crossing t1 t2 (Quantity.sign crossProduct)
  let isSingular = singular1 || singular2
  let alignment = Quantity.sign (tangentVector1 `dot` tangentVector2)
  EndpointIntersection{intersectionPoint, isSingular, alignment}

tangentSignature ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Number ->
  (Vector2d (space @ Unitless), Quantity units, Bool)
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
  Problem (space @ units) ->
  (Bounds Unitless, Bounds Unitless) ->
  Fuzzy (Maybe IntersectionPoint)
findIntersectionPoint problem (tBounds1, tBounds2) = do
  let Problem{curve1, curve2} = problem
  let interiorBounds1 = Curve2d.evaluateBounds curve1 (Bisection.interior tBounds1)
  let interiorBounds2 = Curve2d.evaluateBounds curve2 (Bisection.interior tBounds2)
  if not (interiorBounds1 `intersects` interiorBounds2)
    then Resolved Nothing
    else do
      subdomain1 <- classifySubdomain tBounds1
      subdomain2 <- classifySubdomain tBounds2
      let firstBounds1 = VectorCurve2d.evaluateBounds curve1.derivative tBounds1
      let firstBounds2 = VectorCurve2d.evaluateBounds curve2.derivative tBounds2
      let secondBounds1 = VectorCurve2d.evaluateBounds curve1.derivative.derivative tBounds1
      let secondBounds2 = VectorCurve2d.evaluateBounds curve2.derivative.derivative tBounds2
      let firstCrossProductSign = Bounds.resolvedSign (firstBounds1 `cross#` firstBounds2)
      let uniqueTangentSolution =
            secondDerivativesIndependent firstBounds1 firstBounds2 secondBounds1 secondBounds2
      let isInterior t1 t2 = Bisection.isInterior t1 tBounds1 && Bisection.isInterior t2 tBounds2
      let resolvedEndpointIntersection size allowedAlignment = do
            let isLocal EndpointIntersection{intersectionPoint} =
                  Bounds.includes intersectionPoint.t1 tBounds1
                    && Bounds.includes intersectionPoint.t2 tBounds2
            case List.filter isLocal problem.endpointIntersections of
              [] -> Unresolved
              List.One (EndpointIntersection{intersectionPoint, isSingular, alignment}) ->
                if isSingular && size == Small
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
        (Inner, Inner)
          | Resolved sign <- firstCrossProductSign -> do
              let uvPoint0 = Point2d (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              case NewtonRaphson.surface2d problem.crossingSolutionTarget uvPoint0 of
                Error NewtonRaphson.Divergence -> Unresolved
                Ok (Point2d t1 t2) ->
                  if isInterior t1 t2
                    then Resolved (Just (IntersectionPoint.crossing t1 t2 sign))
                    else Unresolved
          | uniqueTangentSolution -> do
              let uvPoint0 = Point2d (Bounds.midpoint tBounds1) (Bounds.midpoint tBounds2)
              case NewtonRaphson.surface2d problem.tangentSolutionTarget uvPoint0 of
                Error NewtonRaphson.Divergence -> Unresolved
                Ok (Point2d t1 t2) ->
                  if isInterior t1 t2 && Curve2d.evaluate curve1 t1 ~= Curve2d.evaluate curve2 t2
                    then Resolved (Just (IntersectionPoint.tangent t1 t2))
                    else Unresolved
          | otherwise -> Unresolved
        (Inner, Start size) -> resolvedEndpointIntersection size Nothing
        (Inner, End size) -> resolvedEndpointIntersection size Nothing
        (Start size, Inner) -> resolvedEndpointIntersection size Nothing
        (End size, Inner) -> resolvedEndpointIntersection size Nothing
        (Start size1, Start size2) -> resolvedEndpointIntersection (max size1 size2) (Just Negative)
        (Start size1, End size2) -> resolvedEndpointIntersection (max size1 size2) (Just Positive)
        (End size1, Start size2) -> resolvedEndpointIntersection (max size1 size2) (Just Positive)
        (End size1, End size2) -> resolvedEndpointIntersection (max size1 size2) (Just Negative)

secondDerivativesIndependent ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
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
  Bounds (Unitless #/# units)
d2ydx2Bounds x' y' x'' y'' =
  Units.simplify ((y'' #*# x' .-. y' #*# x'') #/# (x' #*# x' #*# x'))

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
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List EndpointIntersection ->
  Maybe (NonEmpty OverlappingSegment)
overlappingSegments curve1 curve2 endpointIntersections =
  case List.concatMap candidateOverlappingSegment (List.successive (,) endpointIntersections) of
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
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Error (Maybe Intersections)
intersections curve1 curve2 = do
  endpointIntersections <- findEndpointIntersections curve1 curve2
  case overlappingSegments curve1 curve2 endpointIntersections of
    Just segments -> Ok (Just (OverlappingSegments segments))
    Nothing -> do
      let u = SurfaceFunction.u
      let v = SurfaceFunction.v
      let curve1Surface = curve1 `compose` u
      let curve2Surface = curve2 `compose` v
      let differenceSurface = curve2Surface .-. curve1Surface
      tangentVector1Surface <- case Curve2d.tangentDirection curve1 of
        Ok tangentDirection1 -> Ok (DirectionCurve2d.unwrap tangentDirection1 `compose` u)
        Error Curve2d.IsPoint -> Error FirstCurveIsPoint
      let problem =
            Problem
              { curve1
              , curve2
              , endpointIntersections
              , crossingSolutionTarget = differenceSurface
              , tangentSolutionTarget =
                  VectorSurfaceFunction2d.xy
                    (differenceSurface `dot` tangentVector1Surface)
                    (SurfaceFunction2d.derivative U curve2Surface `cross` tangentVector1Surface)
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
