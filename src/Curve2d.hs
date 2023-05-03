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
  , overlappingSegments
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import Curve2d.Derivatives (Derivatives (Derivatives))
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import List qualified
import Maybe qualified
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
import VectorBox2d (VectorBox2d)
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

instance IsCurve2d (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  evaluateImpl = evaluate
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

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
  Range.any (nearby point curve) Range.unit |> Result.withDefault False

nearby
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Range Unitless
  -> Result Indeterminate Bool
nearby point curve domain
  | Range.minValue distance > ?tolerance = Ok False
  | Range.maxValue distance <= ?tolerance = Ok True
  | otherwise = Error Indeterminate
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

parameterValues
  :: Tolerance units
  => Point2d (space @ units)
  -> Curve2d (space @ units)
  -> Result Curve2d.IsCoincidentWithPoint (List Float)
parameterValues point curve = do
  roots <-
    let ?tolerance = Qty.squared (Units.generalize ?tolerance)
     in Curve1d.roots (VectorCurve2d.squaredMagnitude (Units.generalize (point - curve)))
          |> Result.mapError (\Curve1d.IsZero -> Curve2d.IsCoincidentWithPoint)
  Ok (List.map Root.value roots)

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List Intersection
  -> List (Range Unitless, Range Unitless)
overlappingSegments curve1 curve2 endpointIntersections =
  let candidateDomains =
        endpointIntersections
          |> List.sortAndDeduplicate
          |> List.successive candidateDomain
   in List.filter (overlappingSegment curve1 curve2) candidateDomains

overlappingSegment
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> (Range Unitless, Range Unitless)
  -> Bool
overlappingSegment curve1 curve2 (domain1, _) =
  let segmentStartPoint = evaluate curve1 (Range.minValue domain1)
      segmentTestPoints = samplingPoints curve1 domain1
      degenerateDomain = List.all (~= segmentStartPoint) segmentTestPoints
   in not degenerateDomain && List.all (\p -> passesThrough p curve2) segmentTestPoints

data IntersectionError
  = BothAreDegenerateAndEqual
  | FirstIsDegenerateOnSecond (List Float)
  | SecondIsDegenerateOnFirst (List Float)
  | OverlappingSegments (List (Range Unitless, Range Unitless))
  | ZeroFirstDerivative
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
  errorMessage ZeroFirstDerivative =
    "Curve first derivative is zero (so tangent direction cannot be easily determined)"

findEndpointIntersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List Intersection)
findEndpointIntersections curve1 curve2 =
  let uValues = do
        v0us <- curve1 |> parameterValues (startPoint curve2)
        v1us <- curve1 |> parameterValues (endPoint curve2)
        Ok (v0us, v1us)
      vValues = do
        u0vs <- curve2 |> parameterValues (startPoint curve1)
        u1vs <- curve2 |> parameterValues (endPoint curve1)
        Ok (u0vs, u1vs)
   in case (uValues, vValues) of
        (Ok (v0us, v1us), Ok (u0vs, u1vs)) ->
          Ok $
            List.concat
              [ List.map (\u -> Intersection u 0.0 Nothing) v0us
              , List.map (\u -> Intersection u 1.0 Nothing) v1us
              , List.map (\v -> Intersection 0.0 v Nothing) u0vs
              , List.map (\v -> Intersection 1.0 v Nothing) u1vs
              ]
        (Error Curve2d.IsCoincidentWithPoint, Ok (u0vs, _)) ->
          Error (FirstIsDegenerateOnSecond u0vs)
        (Ok (v0us, _), Error Curve2d.IsCoincidentWithPoint) ->
          Error (SecondIsDegenerateOnFirst v0us)
        (Error Curve2d.IsCoincidentWithPoint, Error Curve2d.IsCoincidentWithPoint) ->
          Error BothAreDegenerateAndEqual

candidateDomain :: Intersection -> Intersection -> (Range Unitless, Range Unitless)
candidateDomain start end =
  (Range.from start.u end.u, Range.from start.v end.v)

samplingPoints :: Curve2d (space @ units) -> Range Unitless -> List (Point2d (space @ units))
samplingPoints curve domain =
  List.map (Range.interpolate domain >> evaluate curve) Quadrature.parameterValues

newtype AreOverlapping = AreOverlapping (List (Range Unitless, Range Unitless)) deriving (Show)

instance IsError AreOverlapping where
  errorMessage (AreOverlapping _) =
    "Curves are overlapping (so there are infinite intersection points)"

intersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Result IntersectionError (List Intersection)
intersections curve1 curve2 = do
  endpointIntersections <- findEndpointIntersections curve1 curve2
  case overlappingSegments curve1 curve2 endpointIntersections of
    [] ->
      let firstDerivative1 = derivative curve1
          firstDerivative2 = derivative curve2
          secondDerivative1 = VectorCurve2d.derivative firstDerivative1
          secondDerivative2 = VectorCurve2d.derivative firstDerivative2
          derivatives1 = Derivatives curve1 firstDerivative1 secondDerivative1
          derivatives2 = Derivatives curve2 firstDerivative2 secondDerivative2
          innerIntersections =
            findInnerIntersections derivatives1 derivatives2 Range.unit Range.unit
       in Ok (endpointIntersections ++ innerIntersections)
    segments -> Error (OverlappingSegments segments)

findInnerIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> List Intersection
findInnerIntersections derivatives1 derivatives2 u v =
  let solutions = findSolutions derivatives1 derivatives2 u v
   in List.collect (crossingIntersections derivatives1 derivatives2 solutions) solutions
        ++ Maybe.collect tangentIntersection solutions

resolved :: Range units -> Bool
resolved range = Range.resolution range >= 0.5

data Solution
  = Solution0 Intersection
  | Solution1 (Range Unitless) (Range Unitless) (Maybe Intersection)

tangentIntersection :: Solution -> Maybe Intersection
tangentIntersection (Solution0 _) = Nothing
tangentIntersection (Solution1 _ _ maybeIntersection) = maybeIntersection

crossingIntersections
  :: Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List Solution
  -> Solution
  -> List Intersection
crossingIntersections _ _ _ (Solution0 intersection) = [intersection]
crossingIntersections _ _ _ (Solution1 _ _ (Just _)) = []
crossingIntersections derivatives1 derivatives2 solutions (Solution1 u v Nothing)
  | List.any (neighborToTangentIntersection u v) solutions = []
  | otherwise = findCrossingIntersections derivatives1 derivatives2 u v

neighborToTangentIntersection :: Range Unitless -> Range Unitless -> Solution -> Bool
neighborToTangentIntersection _ _ (Solution0 _) = False
neighborToTangentIntersection _ _ (Solution1 _ _ Nothing) = False
neighborToTangentIntersection u1 v1 (Solution1 u2 v2 (Just _)) =
  Range.overlaps u1 u2 && Range.overlaps v1 v2

findSolutions
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> List Solution
findSolutions derivatives1 derivatives2 u v =
  let bounds1 = segmentBounds derivatives1.curve u
      bounds2 = segmentBounds derivatives2.curve v
      difference = bounds1 - bounds2
      distance = VectorBox2d.magnitude difference
   in if distance.minValue > ?tolerance
        then [] -- No roots if bounding boxes are further apart than the given tolerance
        else
          let firstBounds1 = VectorCurve2d.segmentBounds derivatives1.first u
              firstBounds2 = VectorCurve2d.segmentBounds derivatives1.first v
           in if resolvedFirst firstBounds1 firstBounds2
                then List.map Solution0 (findCrossingIntersections derivatives1 derivatives2 u v)
                else
                  let secondBounds1 = VectorCurve2d.segmentBounds derivatives1.second u
                      secondBounds2 = VectorCurve2d.segmentBounds derivatives2.second v
                   in if resolvedSecond firstBounds1 firstBounds2 secondBounds1 secondBounds2
                        then [Solution1 u v (findTangentIntersection derivatives1 derivatives2 u v)]
                        else solveRecursively derivatives1 derivatives2 u v

findTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Maybe Intersection
findTangentIntersection derivatives1 derivatives2 u v =
  case Range.search2 (isTangentIntersection derivatives1 derivatives2) u v of
    [] -> Nothing
    (u0, v0) : _ -> Just (Intersection u0 v0 Nothing)

solveRecursively
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> List Solution
solveRecursively derivatives1 derivatives2 u v =
  let (leftU, rightU) = Range.bisect u
      (leftV, rightV) = Range.bisect v
   in List.concat
        [ findSolutions derivatives1 derivatives2 leftU leftV
        , findSolutions derivatives1 derivatives2 leftU rightV
        , findSolutions derivatives1 derivatives2 rightU leftV
        , findSolutions derivatives1 derivatives2 rightU rightV
        ]

findCrossingIntersections
  :: Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> List Intersection
findCrossingIntersections derivatives1 derivatives2 u v =
  Range.search2 (isCrossingIntersection derivatives1.curve derivatives2.curve) u v
    |> List.map (crossingIntersection derivatives1.first derivatives1.second)

isCrossingIntersection
  :: Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Bool
isCrossingIntersection curve1 curve2 u v =
  BoundingBox2d.overlaps (segmentBounds curve1 u) (segmentBounds curve2 v)

crossingIntersection
  :: VectorCurve2d (space @ units)
  -> VectorCurve2d (space @ units)
  -> (Float, Float)
  -> Intersection
crossingIntersection firstDerivative1 firstDerivative2 (u, v) =
  let first1 = VectorCurve2d.evaluate firstDerivative1 u
      first2 = VectorCurve2d.evaluate firstDerivative2 v
      sign = Qty.sign (Units.generalize first1 >< Units.generalize first2)
   in Intersection u v (Just sign)

resolvedFirst
  :: VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> Bool
resolvedFirst firstBounds1 firstBounds2 =
  resolved (Units.generalize firstBounds1 >< Units.generalize firstBounds2)

resolvedSecond
  :: VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> Bool
resolvedSecond firstBounds1 firstBounds2 secondBounds1 secondBounds2 =
  let dU_X1 = firstBounds1.xComponent
      dU_Y1 = firstBounds1.yComponent
      dV_X2 = firstBounds2.xComponent
      dV_Y2 = firstBounds2.yComponent
      d2U_X1 = secondBounds1.xComponent
      d2U_Y1 = secondBounds1.yComponent
      d2V_X2 = secondBounds2.xComponent
      d2V_Y2 = secondBounds2.yComponent
   in resolvedSecond1d dU_X1 dU_Y1 dV_X2 dV_Y2 d2U_X1 d2U_Y1 d2V_X2 d2V_Y2
        || resolvedSecond1d dU_Y1 dU_X1 dV_Y2 dV_X2 d2U_Y1 d2U_X1 d2V_Y2 d2V_X2

resolvedSecond1d
  :: Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Bool
resolvedSecond1d dU_X1 dU_Y1 dV_X2 dV_Y2 d2U_X1 d2U_Y1 d2V_X2 d2V_Y2
  | resolved dU_X1 && resolved dV_X2 =
      let d2X_Y1 = (d2U_Y1 .* dU_X1 - dU_Y1 .* d2U_X1) ./ (dU_X1 .* dU_X1 .* dU_X1)
          d2X_Y2 = (d2V_Y2 .* dV_X2 - dV_Y2 .* d2V_X2) ./ (dV_X2 .* dV_X2 .* dV_X2)
       in resolved (d2X_Y1 - d2X_Y2)
  | otherwise = False

isTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Bool
isTangentIntersection derivatives1 derivatives2 u v =
  let bounds1 = segmentBounds derivatives1.curve u
      bounds2 = segmentBounds derivatives2.curve v
      difference = bounds1 - bounds2
      distance = VectorBox2d.magnitude difference
   in Range.minValue distance <= ?tolerance
        && let firstBounds1 = VectorCurve2d.segmentBounds derivatives1.first u
               firstBounds2 = VectorCurve2d.segmentBounds derivatives2.first v
               crossProduct = Units.generalize firstBounds1 >< Units.generalize firstBounds2
               dotProduct1 = Units.generalize firstBounds1 <> Units.generalize difference
               dotProduct2 = Units.generalize firstBounds2 <> Units.generalize difference
            in containsZero crossProduct && containsZero dotProduct1 && containsZero dotProduct2

containsZero :: Range units -> Bool
containsZero range = range.minValue <= Qty.zero && range.maxValue >= Qty.zero
