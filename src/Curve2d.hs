module Curve2d
  ( Curve2d (Curve2d, Line, Arc)
  , Intersection
  , IntersectionError (..)
  , IsCurve2d (..)
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  , reverse
  , bisect
  , boundingBox
  , passesThrough
  , intersections
  , parameterValues
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Domain (Domain)
import Domain qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class
  Show curve =>
  IsCurve2d curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateAtImpl :: Float -> curve -> Point2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> BoundingBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)

deriving instance Show (Curve2d coordinateSystem)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (Curve2d (space @ units1'))
      (Curve2d (space' @ units2'))

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _) = p1
startPoint arc@(Arc{}) = evaluateAt 0.0 arc
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2) = p2
endPoint arc@(Arc{}) = evaluateAt 1.0 arc
endPoint (Curve2d curve) = endPointImpl curve

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt t (Line p1 p2) = Point2d.interpolateFrom p1 p2 t
evaluateAt t (Arc p0 r a b) = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluateAt t (Curve2d curve) = evaluateAtImpl t curve

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = evaluateAt t curve

segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
segmentBounds t (Line p1 p2) =
  BoundingBox2d.hull2
    (Point2d.interpolateFrom p1 p2 t.minValue)
    (Point2d.interpolateFrom p1 p2 t.maxValue)
segmentBounds t (Arc p0 r a b) =
  let theta = a + t * (b - a) in p0 + VectorBox2d.polar (Range.constant r) theta
segmentBounds t (Curve2d curve) = segmentBoundsImpl t curve

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ r a b) =
  let theta = a + Curve1d.parameter * (b - a)
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)
derivative (Curve2d curve) = derivativeImpl curve

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line p1 p2) = Line p2 p1
reverse (Arc p0 r a b) = Arc p0 r b a
reverse (Curve2d curve) = Curve2d (reverseImpl curve)

bisect :: Curve2d (space @ units) -> (Curve2d (space @ units), Curve2d (space @ units))
bisect (Line p1 p2) = let mid = Point2d.midpoint p1 p2 in (Line p1 mid, Line mid p2)
bisect (Arc p0 r a b) = let mid = Qty.midpoint a b in (Arc p0 r a mid, Arc p0 r mid b)
bisect (Curve2d curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d curve1, Curve2d curve2)

boundingBox :: Curve2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox (Line p1 p2) = BoundingBox2d.hull2 p1 p2
boundingBox arc@(Arc{}) = segmentBounds Domain.unit arc
boundingBox (Curve2d curve) = boundingBoxImpl curve

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - evaluateAt t curve
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds t curve
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Point2d (space @ units))
      (Curve2d (space' @ units'))
      (VectorCurve2d (space @ units))
  where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem)
  = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

instance IsVectorCurve2d (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = evaluateAt t curve - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds t curve - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Curve2d (space @ units))
      (Point2d (space' @ units'))
      (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data CurveIsCoincidentWithPoint = CurveIsCoincidentWithPoint deriving (Eq, Show, ErrorMessage)

passesThrough :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
passesThrough point curve =
  Range.any (segmentIsCoincidentWithPoint point curve) Domain.unit

segmentIsCoincidentWithPoint
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Domain
  -> Fuzzy Bool
segmentIsCoincidentWithPoint point curve domain
  | Range.minValue distance > ?tolerance = Resolved False
  | Range.maxValue distance <= ?tolerance = Resolved True
  | otherwise = Unresolved
 where
  distance = VectorBox2d.magnitude (point - segmentBounds domain curve)

parameterValues
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Result Curve2d.CurveIsCoincidentWithPoint (List Float)
parameterValues point curve =
  let ?tolerance = Qty.squared (Units.generalize ?tolerance)
   in Curve1d.roots (VectorCurve2d.squaredMagnitude (Units.generalize (point - curve)))
        |> Result.mapError (\Curve1d.ZeroEverywhere -> Curve2d.CurveIsCoincidentWithPoint)
        |> Result.map (List.map Root.value)

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> List (Domain, Domain)
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.sortAndDeduplicate
    |> List.successive (\(u1, v1) (u2, v2) -> (Range.from u1 u2, Range.from v1 v2))
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> (Domain, Domain)
  -> Bool
isOverlappingSegment curve1 curve2 (domain1, _) =
  let segmentStartPoint = evaluateAt (Range.minValue domain1) curve1
      curve1TestPoints = Domain.sample (pointOn curve1) domain1
      segment1IsNondegenerate = List.any (!= segmentStartPoint) curve1TestPoints
      segment1LiesOnSegment2 = List.all (\p1 -> passesThrough p1 curve2) curve1TestPoints
   in segment1IsNondegenerate && segment1LiesOnSegment2

data IntersectionError
  = CurvesAreDegenerateAndEqual
  | FirstCurveIsDegenerateOnSecond (List Float)
  | SecondCurveIsDegenerateOnFirst (List Float)
  | CurvesOverlap (List (Domain, Domain))
  | CurveDerivativesAreZero
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, ErrorMessage)

findEndpointParameterValues
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List (Float, Float))
findEndpointParameterValues curve1 curve2 =
  let uValues = do
        v0us <- parameterValues (startPoint curve2) curve1
        v1us <- parameterValues (endPoint curve2) curve1
        Ok (v0us, v1us)
      vValues = do
        u0vs <- parameterValues (startPoint curve1) curve2
        u1vs <- parameterValues (endPoint curve1) curve2
        Ok (u0vs, u1vs)
   in case (uValues, vValues) of
        (Ok (v0us, v1us), Ok (u0vs, u1vs)) ->
          Ok $
            List.concat
              [ List.map (,0.0) v0us
              , List.map (,1.0) v1us
              , List.map (0.0,) u0vs
              , List.map (1.0,) u1vs
              ]
        (Error Curve2d.CurveIsCoincidentWithPoint, Ok (u0vs, _)) ->
          Error (FirstCurveIsDegenerateOnSecond u0vs)
        (Ok (v0us, _), Error Curve2d.CurveIsCoincidentWithPoint) ->
          Error (SecondCurveIsDegenerateOnFirst v0us)
        (Error Curve2d.CurveIsCoincidentWithPoint, Error Curve2d.CurveIsCoincidentWithPoint) ->
          Error CurvesAreDegenerateAndEqual

curveDerivatives
  :: Tolerance units
  => Curve2d (space @ units)
  -> Result IntersectionError (Derivatives (space @ units))
curveDerivatives curve =
  Derivatives.ofCurve curve |> Result.mapError (\Derivatives.AreZero -> Curve2d.CurveDerivativesAreZero)

intersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List Intersection)
intersections curve1 curve2 = do
  endpointParameterValues <- findEndpointParameterValues curve1 curve2
  case overlappingSegments curve1 curve2 endpointParameterValues of
    [] -> findIntersections curve1 curve2 endpointParameterValues
    segments -> Error (CurvesOverlap segments)

endpointIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> (Float, Float)
  -> Result IntersectionError Intersection
endpointIntersection derivatives1 derivatives2 (u1, u2) =
  Derivatives.classify derivatives1 derivatives2 u1 u2
    |> Result.map (\(kind, sign) -> Intersection u1 u2 kind sign)
    |> Result.mapError
      (\Derivatives.DegenerateIntersection -> Curve2d.TangentIntersectionAtDegeneratePoint)

findIntersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> Result IntersectionError (List Intersection)
findIntersections curve1 curve2 endpointParameterValues = do
  derivatives1 <- curveDerivatives curve1
  derivatives2 <- curveDerivatives curve2
  endpointIntersections <-
    Result.collect (endpointIntersection derivatives1 derivatives2) endpointParameterValues
  let endpointRegions =
        findEndpointRegions derivatives1 derivatives2 endpointIntersections Domain.unit Domain.unit []
  let (tangentIntersections, tangentRegions) =
        List.unzip2 $
          findTangentSolutions derivatives1 derivatives2 endpointRegions Domain.unit Domain.unit []
  let crossingIntersections =
        findCrossingIntersections
          derivatives1
          derivatives2
          (endpointRegions ++ tangentRegions)
          Domain.unit
          Domain.unit
          []
  Ok (endpointIntersections ++ tangentIntersections ++ crossingIntersections)

findCrossingIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Domain, Domain)
  -> Domain
  -> Domain
  -> List Intersection
  -> List Intersection
findCrossingIntersections derivatives1 derivatives2 exclusionRegions u1 u2 accumulated =
  let filteredRegions = List.filter (intersectingRegions (u1, u2)) exclusionRegions
      findInSubdomains = findCrossingIntersections derivatives1 derivatives2 filteredRegions
   in case Derivatives.intersectionType derivatives1 derivatives2 u1 u2 of
        Resolved Nothing -> accumulated
        Resolved (Just (Intersection.Tangent, _)) -> accumulated
        Resolved (Just (Intersection.Crossing, sign)) ->
          case filteredRegions of
            [] -> findCrossingIntersection sign derivatives1 derivatives2 u1 u2 ++ accumulated
            _ -> Range.recurse2 findInSubdomains u1 u2 accumulated
        Unresolved -> Range.recurse2 findInSubdomains u1 u2 accumulated

findCrossingIntersection
  :: Sign
  -> Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> List Intersection
findCrossingIntersection sign derivatives1 derivatives2 u1 u2 =
  Range.search2 (isCrossingIntersection derivatives1.curve derivatives2.curve) u1 u2
    |> List.map (\(u1', u2') -> Intersection u1' u2' Intersection.Crossing sign)

isCrossingIntersection
  :: Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Domain
  -> Domain
  -> Bool
isCrossingIntersection curve1 curve2 u1 u2 =
  BoundingBox2d.intersects (segmentBounds u1 curve1) (segmentBounds u2 curve2)

findTangentSolutions
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Domain, Domain)
  -> Domain
  -> Domain
  -> List (Intersection, (Domain, Domain))
  -> List (Intersection, (Domain, Domain))
findTangentSolutions derivatives1 derivatives2 endpointRegions u1 u2 accumulated =
  let filteredRegions = List.filter (intersectingRegions (u1, u2)) endpointRegions
      findInSubdomains = findTangentSolutions derivatives1 derivatives2 filteredRegions
   in case Derivatives.intersectionType derivatives1 derivatives2 u1 u2 of
        Resolved Nothing -> accumulated
        Resolved (Just (Intersection.Crossing, _)) -> accumulated
        Resolved (Just (Intersection.Tangent, sign)) ->
          case filteredRegions of
            [] ->
              case findTangentIntersection sign derivatives1 derivatives2 u1 u2 of
                Just intersection -> (intersection, (u1, u2)) : accumulated
                Nothing -> accumulated
            _ -> Range.recurse2 findInSubdomains u1 u2 accumulated
        Unresolved -> Range.recurse2 findInSubdomains u1 u2 accumulated

findTangentIntersection
  :: Tolerance units
  => Sign
  -> Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> Maybe Intersection
findTangentIntersection sign derivatives1 derivatives2 u1 u2 =
  case Range.search2 (isTangentIntersection derivatives1 derivatives2) u1 u2 of
    [] -> Nothing
    (u1', u2') : _ -> Just (Intersection u1' u2' Intersection.Tangent sign)

isTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> Bool
isTangentIntersection derivatives1 derivatives2 u1 u2 =
  let bounds1 = segmentBounds u1 derivatives1.curve
      bounds2 = segmentBounds u2 derivatives2.curve
      difference = bounds1 - bounds2
      distance = VectorBox2d.magnitude difference
   in Range.minValue distance <= ?tolerance
        && let firstBounds1 = VectorCurve2d.segmentBounds u1 derivatives1.first
               firstBounds2 = VectorCurve2d.segmentBounds u2 derivatives2.first
               crossProduct = Units.generalize firstBounds1 >< Units.generalize firstBounds2
               dotProduct1 = Units.generalize firstBounds1 <> Units.generalize difference
               dotProduct2 = Units.generalize firstBounds2 <> Units.generalize difference
            in Range.includes Qty.zero crossProduct
                && Range.includes Qty.zero dotProduct1
                && Range.includes Qty.zero dotProduct2

intersectingRegions :: (Domain, Domain) -> (Domain, Domain) -> Bool
intersectingRegions (a1, a2) (b1, b2) = Range.intersects a1 b1 && Range.intersects a2 b2

findEndpointRegions
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List Intersection
  -> Domain
  -> Domain
  -> List (Domain, Domain)
  -> List (Domain, Domain)
findEndpointRegions derivatives1 derivatives2 endpointIntersections u1 u2 accumulated =
  case List.filter (containsIntersection u1 u2) endpointIntersections of
    [] -> accumulated
    filteredIntersections ->
      let expanded1 = Domain.expand u1
          expanded2 = Domain.expand u2
          findInSubdomains = findEndpointRegions derivatives1 derivatives2 filteredIntersections
       in case Derivatives.intersectionType derivatives1 derivatives2 expanded1 expanded2 of
            Resolved (Just (regionKind, regionSign)) ->
              if List.all (\intersection -> intersection.kind == regionKind && intersection.sign == regionSign) filteredIntersections
                then (expanded1, expanded2) : accumulated
                else Range.recurse2 findInSubdomains u1 u2 accumulated
            Resolved Nothing -> accumulated
            Unresolved -> Range.recurse2 findInSubdomains u1 u2 accumulated

containsIntersection :: Domain -> Domain -> Intersection -> Bool
containsIntersection u1 u2 intersection =
  Range.includes intersection.u1 u1 && Range.includes intersection.u2 u2
