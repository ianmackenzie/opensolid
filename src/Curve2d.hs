module Curve2d
  ( Curve2d (Line, Arc)
  , DegenerateCurve (DegenerateCurve)
  , Intersection
  , IntersectionError (..)
  , IsCurve2d (..)
  , unsafeLine
  , unsafeArc
  , from
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
import Bisection qualified
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.SearchBox (SearchBox)
import Curve2d.SearchBox qualified as SearchBox
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
  Line# :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc# :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d# :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)

pattern Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
pattern Line startPoint endPoint <- Line# startPoint endPoint

pattern Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
pattern Arc centerPoint radius startAngle endAngle <- Arc# centerPoint radius startAngle endAngle

deriving instance Show (Curve2d coordinateSystem)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (Curve2d (space @ units1'))
      (Curve2d (space' @ units2'))

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, ErrorMessage)

unsafeLine :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
unsafeLine = Line#

unsafeArc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
unsafeArc = Arc#

from
  :: Tolerance units
  => IsCurve2d curve (space @ units)
  => curve
  -> Result DegenerateCurve (Curve2d (space @ units))
from curve =
  let firstDerivative = derivativeImpl curve
      secondDerivative = VectorCurve2d.derivative firstDerivative
   in if Range.any (areBothZero firstDerivative secondDerivative) Domain.unit
        then Error DegenerateCurve
        else Ok (Curve2d# curve)

areBothZero
  :: Tolerance units
  => VectorCurve2d (space @ units)
  -> VectorCurve2d (space @ units)
  -> Domain
  -> Fuzzy Bool
areBothZero firstDerivative secondDerivative domain
  | firstMin > ?tolerance || 0.5 * secondMin > ?tolerance = Resolved False
  | firstMax <= ?tolerance && 0.5 * secondMax <= ?tolerance = Resolved True
  | otherwise = Unresolved
 where
  firstBounds = VectorCurve2d.segmentBounds domain firstDerivative
  secondBounds = VectorCurve2d.segmentBounds domain secondDerivative
  (Range firstMin firstMax) = VectorBox2d.magnitude firstBounds
  (Range secondMin secondMax) = VectorBox2d.magnitude secondBounds

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line# p1 _) = p1
startPoint arc@(Arc#{}) = evaluateAt 0.0 arc
startPoint (Curve2d# curve) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line# _ p2) = p2
endPoint arc@(Arc#{}) = evaluateAt 1.0 arc
endPoint (Curve2d# curve) = endPointImpl curve

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt t (Line# p1 p2) = Point2d.interpolateFrom p1 p2 t
evaluateAt t (Arc# p0 r a b) = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluateAt t (Curve2d# curve) = evaluateAtImpl t curve

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = evaluateAt t curve

segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
segmentBounds t (Line# p1 p2) =
  BoundingBox2d.hull2
    (Point2d.interpolateFrom p1 p2 t.minValue)
    (Point2d.interpolateFrom p1 p2 t.maxValue)
segmentBounds t (Arc# p0 r a b) =
  let theta = a + t * (b - a) in p0 + VectorBox2d.polar (Range.constant r) theta
segmentBounds t (Curve2d# curve) = segmentBoundsImpl t curve

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line# p1 p2) = VectorCurve2d.constant (p2 - p1)
derivative (Arc# _ r a b) =
  let theta = a + Curve1d.parameter * (b - a)
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)
derivative (Curve2d# curve) = derivativeImpl curve

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line# p1 p2) = unsafeLine p2 p1
reverse (Arc# p0 r a b) = unsafeArc p0 r b a
reverse (Curve2d# curve) = Curve2d# (reverseImpl curve)

bisect :: Curve2d (space @ units) -> (Curve2d (space @ units), Curve2d (space @ units))
bisect (Line# p1 p2) = let mid = Point2d.midpoint p1 p2 in (unsafeLine p1 mid, unsafeLine mid p2)
bisect (Arc# p0 r a b) = let mid = Qty.midpoint a b in (unsafeArc p0 r a mid, unsafeArc p0 r mid b)
bisect (Curve2d# curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d# curve1, Curve2d# curve2)

boundingBox :: Curve2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox (Line# p1 p2) = BoundingBox2d.hull2 p1 p2
boundingBox arc@(Arc#{}) = segmentBounds Domain.unit arc
boundingBox (Curve2d# curve) = boundingBoxImpl curve

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
  -> List Float
parameterValues point curve =
  case Curve1d.roots (VectorCurve2d.squaredMagnitude (Units.generalize (point - curve))) of
    Ok roots -> List.map Root.value roots
    Error Curve1d.ZeroEverywhere -> [] -- Shouldn't happen, since curves are enforced to be non-degenerate
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> List (Domain, Domain)
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
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
  = CurvesOverlap (List (Domain, Domain))
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, ErrorMessage)

findEndpointParameterValues
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
findEndpointParameterValues curve1 curve2 =
  List.sortAndDeduplicate $
    List.concat
      [ List.map (0.0,) (parameterValues (startPoint curve1) curve2)
      , List.map (1.0,) (parameterValues (endPoint curve1) curve2)
      , List.map (,0.0) (parameterValues (startPoint curve2) curve1)
      , List.map (,1.0) (parameterValues (endPoint curve2) curve1)
      ]

intersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List Intersection)
intersections curve1 curve2 = do
  let endpointParameterValues = findEndpointParameterValues curve1 curve2
  case overlappingSegments curve1 curve2 endpointParameterValues of
    [] -> findIntersections curve1 curve2 endpointParameterValues
    segments -> Error (CurvesOverlap segments)

type SearchTree (coordinateSystem :: CoordinateSystem) =
  Bisection.Quadtree (SearchBox coordinateSystem)

findIntersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> Result IntersectionError (List Intersection)
findIntersections curve1 curve2 endpointParameterValues = do
  let derivatives1 = Derivatives.ofCurve curve1
  let derivatives2 = Derivatives.ofCurve curve2
  let searchTree = Bisection.quadtree (SearchBox.init derivatives1 derivatives2)
  endpointResults <-
    findEndpointIntersections derivatives1 derivatives2 endpointParameterValues searchTree ([], [])
  let (allIntersections, _) =
        endpointResults
          |> findTangentIntersections derivatives1 derivatives2 searchTree
          |> findCrossingIntersections derivatives1 derivatives2 searchTree
  Ok (List.sort allIntersections)

findEndpointIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Float, Float)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> Result IntersectionError (List Intersection, List (Domain, Domain))
findEndpointIntersections _ _ [] _ accumulated = Ok accumulated
findEndpointIntersections derivatives1 derivatives2 (uv : rest) searchTree accumulated = do
  updated <- findEndpointIntersection derivatives1 derivatives2 uv searchTree accumulated
  findEndpointIntersections derivatives1 derivatives2 rest searchTree updated

findEndpointIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> (Float, Float)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> Result IntersectionError (List Intersection, List (Domain, Domain))
findEndpointIntersection derivatives1 derivatives2 uv searchTree accumulated = do
  intersectionType <-
    Derivatives.classify uv derivatives1 derivatives2
      |> Result.mapError (\Intersection.TangentIntersectionAtDegeneratePoint -> TangentIntersectionAtDegeneratePoint)
  let (intersectionKind, sign) = intersectionType
  let (u0, v0) = uv
  Ok $
    Bisection.solve2
      (SearchBox.isEndpointIntersectionCandidate uv)
      (SearchBox.endpointIntersectionResolved intersectionType)
      (\_ _ _ _ -> Just (Intersection u0 v0 intersectionKind sign))
      searchTree
      accumulated

findTangentIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> (List Intersection, List (Domain, Domain))
findTangentIntersections derivatives1 derivatives2 =
  Bisection.solve2
    SearchBox.isTangentIntersectionCandidate
    SearchBox.tangentIntersectionSign
    (SearchBox.findTangentIntersection derivatives1 derivatives2)

findCrossingIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> (List Intersection, List (Domain, Domain))
findCrossingIntersections derivatives1 derivatives2 =
  Bisection.solve2
    SearchBox.isCrossingIntersectionCandidate
    SearchBox.crossingIntersectionSign
    (SearchBox.findCrossingIntersection derivatives1 derivatives2)
