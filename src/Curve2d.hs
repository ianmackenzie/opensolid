module Curve2d
  ( Curve2d (Curve2d, Line, Arc)
  , Intersection
  , IntersectionError (..)
  , IsCurve2d (..)
  , startPoint
  , endPoint
  , evaluate
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
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Quadrature qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units (Unitless)
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
  evaluateImpl :: curve -> Float -> Point2d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d coordinateSystem
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
startPoint arc@(Arc{}) = evaluate arc 0.0
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2) = p2
endPoint arc@(Arc{}) = evaluate arc 1.0
endPoint (Curve2d curve) = endPointImpl curve

evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluate (Line p1 p2) t = Point2d.interpolateFrom p1 p2 t
evaluate (Arc p0 r a b) t = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluate (Curve2d curve) t = evaluateImpl curve t

segmentBounds :: Curve2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds (Line p1 p2) t =
  BoundingBox2d.hull2
    (Point2d.interpolateFrom p1 p2 t.minValue)
    (Point2d.interpolateFrom p1 p2 t.maxValue)
segmentBounds (Arc p0 r a b) t =
  let theta = a + t * (b - a) in p0 + VectorBox2d.polar (Range.constant r) theta
segmentBounds (Curve2d curve) t = segmentBoundsImpl curve t

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
boundingBox arc@(Arc{}) = segmentBounds arc Range.unit
boundingBox (Curve2d curve) = boundingBoxImpl curve

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (space @ units)) (space @ units) where
  evaluateImpl (PointCurveDifference point curve) t = point - evaluate curve t
  segmentBoundsImpl (PointCurveDifference point curve) t = point - segmentBounds curve t
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
  evaluateImpl (CurvePointDifference curve point) t = evaluate curve t - point
  segmentBoundsImpl (CurvePointDifference curve point) t = segmentBounds curve t - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Curve2d (space @ units))
      (Point2d (space' @ units'))
      (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

instance IsError IsCoincidentWithPoint where
  errorMessage IsCoincidentWithPoint =
    "Curve is in fact a single point coincident with the given point"

passesThrough :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
passesThrough point curve =
  case Range.any (segmentIsCoincidentWithPoint point curve) Range.unit of
    Resolved result -> result
    Unresolved -> False

segmentIsCoincidentWithPoint
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Range Unitless
  -> Fuzzy Bool
segmentIsCoincidentWithPoint point curve domain
  | Range.minValue distance > ?tolerance = Resolved False
  | Range.maxValue distance <= ?tolerance = Resolved True
  | otherwise = Unresolved
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

parameterValues
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Result Curve2d.IsCoincidentWithPoint (List Float)
parameterValues point curve =
  let ?tolerance = Qty.squared (Units.generalize ?tolerance)
   in Curve1d.roots (VectorCurve2d.squaredMagnitude (Units.generalize (point - curve)))
        |> Result.mapError (\Curve1d.IsZero -> Curve2d.IsCoincidentWithPoint)
        |> Result.map (List.map Root.value)

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> List (Range Unitless, Range Unitless)
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.sortAndDeduplicate
    |> List.successive (\(u1, v1) (u2, v2) -> (Range.from u1 u2, Range.from v1 v2))
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> (Range Unitless, Range Unitless)
  -> Bool
isOverlappingSegment curve1 curve2 (domain1, _) =
  let segmentStartPoint = evaluate curve1 (Range.minValue domain1)
      segmentTestPoints = samplingPoints curve1 domain1
   in not (List.all (\p -> p ~= segmentStartPoint) segmentTestPoints)
        && List.all (\p -> passesThrough p curve2) segmentTestPoints

data IntersectionError
  = BothAreDegenerateAndEqual
  | FirstIsDegenerateOnSecond (List Float)
  | SecondIsDegenerateOnFirst (List Float)
  | OverlappingSegments (List (Range Unitless, Range Unitless))
  | ZeroDerivatives
  | TangentIntersectionAtDegeneratePoint
  deriving (Show)

instance IsError IntersectionError where
  errorMessage BothAreDegenerateAndEqual =
    "Both curves are a single point each and are are equal to each other"
  errorMessage (FirstIsDegenerateOnSecond _) =
    "First curve is a single point on the second curve"
  errorMessage (SecondIsDegenerateOnFirst _) =
    "Second curve is a single point on the first curve"
  errorMessage (OverlappingSegments _) =
    "Curves have overlapping segments"
  errorMessage ZeroDerivatives =
    "Both first and second curve derivatives are zero"
  errorMessage TangentIntersectionAtDegeneratePoint =
    "Tangent intersection where a curve first derivative is zero"

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
        (Error Curve2d.IsCoincidentWithPoint, Ok (u0vs, _)) ->
          Error (FirstIsDegenerateOnSecond u0vs)
        (Ok (v0us, _), Error Curve2d.IsCoincidentWithPoint) ->
          Error (SecondIsDegenerateOnFirst v0us)
        (Error Curve2d.IsCoincidentWithPoint, Error Curve2d.IsCoincidentWithPoint) ->
          Error BothAreDegenerateAndEqual

samplingPoints :: Curve2d (space @ units) -> Range Unitless -> List (Point2d (space @ units))
samplingPoints curve domain =
  List.map (Range.interpolate domain >> evaluate curve) Quadrature.parameterValues

curveDerivatives
  :: Tolerance units
  => Curve2d (space @ units)
  -> Result IntersectionError (Derivatives (space @ units))
curveDerivatives curve =
  Derivatives.ofCurve curve |> Result.mapError (\Derivatives.AreZero -> Curve2d.ZeroDerivatives)

intersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List Intersection)
intersections curve1 curve2 = do
  endpointParameterValues <- findEndpointParameterValues curve1 curve2
  case overlappingSegments curve1 curve2 endpointParameterValues of
    [] -> findIntersections curve1 curve2 endpointParameterValues
    segments -> Error (OverlappingSegments segments)

endpointIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> (Float, Float)
  -> Result IntersectionError Intersection
endpointIntersection derivatives1 derivatives2 (u1, u2) =
  Derivatives.classify derivatives1 derivatives2 u1 u2
    |> Result.map (Intersection u1 u2)
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
        findEndpointRegions derivatives1 derivatives2 endpointIntersections Range.unit Range.unit []
  let (tangentIntersections, tangentRegions) =
        List.unzip2 $
          findTangentSolutions derivatives1 derivatives2 endpointRegions Range.unit Range.unit []
  let crossingIntersections =
        findCrossingIntersections
          derivatives1
          derivatives2
          (endpointRegions ++ tangentRegions)
          Range.unit
          Range.unit
          []
  Ok (endpointIntersections ++ tangentIntersections ++ crossingIntersections)

findCrossingIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Range Unitless, Range Unitless)
  -> Range Unitless
  -> Range Unitless
  -> List Intersection
  -> List Intersection
findCrossingIntersections derivatives1 derivatives2 exclusionRegions u1 u2 accumulated =
  let filteredRegions = List.filter (intersectingRegions (u1, u2)) exclusionRegions
      findInSubdomains = findCrossingIntersections derivatives1 derivatives2 filteredRegions
   in case Derivatives.intersectionKind derivatives1 derivatives2 u1 u2 of
        Resolved Nothing -> accumulated
        Resolved (Just (Intersection.Tangent _)) -> accumulated
        Resolved (Just (Intersection.Crossing sign)) ->
          case filteredRegions of
            [] -> findCrossingIntersection sign derivatives1 derivatives2 u1 u2 ++ accumulated
            _ -> Range.recurse2 findInSubdomains u1 u2 accumulated
        Unresolved -> Range.recurse2 findInSubdomains u1 u2 accumulated

findCrossingIntersection
  :: Sign
  -> Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> List Intersection
findCrossingIntersection sign derivatives1 derivatives2 u1 u2 =
  Range.search2 (isCrossingIntersection derivatives1.curve derivatives2.curve) u1 u2
    |> List.map (\(u1', u2') -> Intersection u1' u2' (Intersection.Crossing sign))

isCrossingIntersection
  :: Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Bool
isCrossingIntersection curve1 curve2 u1 u2 =
  BoundingBox2d.intersects (segmentBounds curve1 u1) (segmentBounds curve2 u2)

findTangentSolutions
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Range Unitless, Range Unitless)
  -> Range Unitless
  -> Range Unitless
  -> List (Intersection, (Range Unitless, Range Unitless))
  -> List (Intersection, (Range Unitless, Range Unitless))
findTangentSolutions derivatives1 derivatives2 endpointRegions u1 u2 accumulated =
  let filteredRegions = List.filter (intersectingRegions (u1, u2)) endpointRegions
      findInSubdomains = findTangentSolutions derivatives1 derivatives2 filteredRegions
   in case Derivatives.intersectionKind derivatives1 derivatives2 u1 u2 of
        Resolved Nothing -> accumulated
        Resolved (Just (Intersection.Crossing _)) -> accumulated
        Resolved (Just (Intersection.Tangent sign)) ->
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
  -> Range Unitless
  -> Range Unitless
  -> Maybe Intersection
findTangentIntersection sign derivatives1 derivatives2 u1 u2 =
  case Range.search2 (isTangentIntersection derivatives1 derivatives2) u1 u2 of
    [] -> Nothing
    (u1', u2') : _ -> Just (Intersection u1' u2' (Intersection.Tangent sign))

isTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Bool
isTangentIntersection derivatives1 derivatives2 u1 u2 =
  let bounds1 = segmentBounds derivatives1.curve u1
      bounds2 = segmentBounds derivatives2.curve u2
      difference = bounds1 - bounds2
      distance = VectorBox2d.magnitude difference
   in Range.minValue distance <= ?tolerance
        && let firstBounds1 = VectorCurve2d.segmentBounds derivatives1.first u1
               firstBounds2 = VectorCurve2d.segmentBounds derivatives2.first u2
               crossProduct = Units.generalize firstBounds1 >< Units.generalize firstBounds2
               dotProduct1 = Units.generalize firstBounds1 <> Units.generalize difference
               dotProduct2 = Units.generalize firstBounds2 <> Units.generalize difference
            in Range.includes Qty.zero crossProduct
                && Range.includes Qty.zero dotProduct1
                && Range.includes Qty.zero dotProduct2

intersectingRegions :: (Range Unitless, Range Unitless) -> (Range Unitless, Range Unitless) -> Bool
intersectingRegions (a1, a2) (b1, b2) = Range.intersects a1 b1 && Range.intersects a2 b2

expand :: Range Unitless -> Range Unitless
expand (Range low high) =
  let expansion = 0.5 * (high - low)
   in Range.unsafe (max 0.0 (low - expansion)) (min 1.0 (high + expansion))

findEndpointRegions
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List Intersection
  -> Range Unitless
  -> Range Unitless
  -> List (Range Unitless, Range Unitless)
  -> List (Range Unitless, Range Unitless)
findEndpointRegions derivatives1 derivatives2 endpointIntersections u1 u2 accumulated =
  case List.filter (containsIntersection u1 u2) endpointIntersections of
    [] -> accumulated
    filteredIntersections ->
      let expanded1 = expand u1
          expanded2 = expand u2
          findInSubdomains = findEndpointRegions derivatives1 derivatives2 filteredIntersections
       in case Derivatives.intersectionKind derivatives1 derivatives2 expanded1 expanded2 of
            Resolved (Just regionKind) ->
              if List.all (\intersection -> intersection.kind == regionKind) filteredIntersections
                then (expanded1, expanded2) : accumulated
                else Range.recurse2 findInSubdomains u1 u2 accumulated
            Resolved Nothing -> accumulated
            Unresolved -> Range.recurse2 findInSubdomains u1 u2 accumulated

containsIntersection :: Range Unitless -> Range Unitless -> Intersection -> Bool
containsIntersection u1 u2 intersection =
  Range.includes intersection.u1 u1 && Range.includes intersection.u2 u2
