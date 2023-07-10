module Curve2d
  ( Curve2d
  , pattern Line
  , pattern Arc
  , DegenerateCurve (DegenerateCurve)
  , Intersection
  , IntersectionError (..)
  , IsCurve2d (..)
  , from
  , startPoint
  , endPoint
  , evaluateAt
  , pointOn
  , tangentDirectionAt
  , tangentDirectionBounds
  , segmentBounds
  , derivative
  , reverse
  , boundingBox
  , passesThrough
  , intersections
  , parameterValues
  )
where

import Angle (Angle)
import Angle qualified
import Bisection qualified
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Internal (IsCurve2d (..))
import Curve2d.Internal qualified as Internal
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import Direction2d (Direction2d)
import Direction2d qualified
import Domain (Domain)
import Domain qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units (Unitless)
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

pattern Line
  :: Point2d (space @ units)
  -> Point2d (space @ units)
  -> Direction2d space
  -> Curve2d (space @ units)
pattern Line startPoint endPoint direction <-
  Internal.Line startPoint endPoint direction

pattern Arc
  :: Point2d (space @ units)
  -> Qty units
  -> Angle
  -> Angle
  -> Curve2d (space @ units)
pattern Arc centerPoint radius startAngle endAngle <-
  Internal.Arc centerPoint radius startAngle endAngle

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, ErrorMessage)

from
  :: Tolerance units
  => IsCurve2d curve (space @ units)
  => curve
  -> Result DegenerateCurve (Curve2d (space @ units))
from curve =
  let firstDerivative = derivativeImpl curve
      secondDerivative = VectorCurve2d.derivative firstDerivative
   in if isNondegenerate firstDerivative secondDerivative
        then Ok (Internal.Curve curve ?tolerance firstDerivative secondDerivative)
        else Error DegenerateCurve

isNondegenerate :: Tolerance units => VectorCurve2d (space @ units) -> VectorCurve2d (space @ units) -> Bool
isNondegenerate firstDerivative secondDerivative =
  case VectorCurve2d.roots firstDerivative of
    Error VectorCurve2d.ZeroEverywhere -> False
    Ok roots -> List.all (isRemovableDegeneracy secondDerivative) roots

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy secondDerivative t =
  (t == 0.0 || t == 1.0) && secondDerivativeIsNonZero (VectorCurve2d.evaluateAt t secondDerivative)

secondDerivativeIsNonZero :: Tolerance units => Vector2d (space @ units) -> Bool
secondDerivativeIsNonZero secondDerivative =
  2.0 * Vector2d.squaredMagnitude (Units.generalize secondDerivative) != Qty.zero
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint = Internal.endPoint

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt = Internal.evaluateAt

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = evaluateAt t curve

segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
segmentBounds = Internal.segmentBounds

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative = Internal.derivative

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse = Internal.reverse

boundingBox :: Curve2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox (Internal.Line p1 p2 _) = BoundingBox2d.hull2 p1 p2
boundingBox arc@(Internal.Arc{}) = segmentBounds Domain.unit arc
boundingBox (Internal.Curve curve _ _ _) = boundingBoxImpl curve

tangentDirectionAt :: Float -> Curve2d (space @ units) -> Direction2d space
tangentDirectionAt _ (Internal.Line _ _ direction) = direction
tangentDirectionAt t (Internal.Arc _ _ a b) =
  Direction2d.fromAngle (Qty.interpolateFrom a b t + Angle.quarterTurn * Qty.sign (b - a))
tangentDirectionAt t (Internal.Curve _ tolerance first second) =
  -- Find the tangent direction of a general curve, using the first derivative if possible
  -- and falling back to the second derivative at degenerate endpoints
  -- (endpoints where the first derivative is zero).
  -- For consistency, use the tolerance we used during curve construction to check for degeneracies
  -- (and stored within the curve value for this purpose).
  let ?tolerance = tolerance
   in case Vector2d.direction (VectorCurve2d.evaluateAt t first) of
        Ok direction -> direction -- First derivative was non-zero
        Error Vector2d.IsZero ->
          -- First derivative was (approximately) zero, so we must be near a degenerate endpoint.
          -- In this case, estimate the tangent using the second derivative instead.
          let
            -- Estimate tangent direction using second derivative evaluated at a point t',
            -- which is halfway between t and the nearby endpoint
            t' = if t <= 0.5 then 0.5 * t else t + 0.5 * (1.0 - t)
            -- Near the start point (assuming the start point is degenerate),
            -- the tangent direction is in the same direction as the second derivative;
            -- near the end point, it is in the *opposite* direction
            sign = if t <= 0.5 then Positive else Negative
           in
            sign * Direction2d.unsafe (Vector2d.normalize (VectorCurve2d.evaluateAt t' second))

tangentDirectionBounds :: Domain -> Curve2d (space @ units) -> VectorBox2d (space @ Unitless)
tangentDirectionBounds _ (Internal.Line _ _ direction) = VectorBox2d.constant (Direction2d.unwrap direction)
tangentDirectionBounds t (Internal.Arc _ _ a b) =
  let rotation = Angle.quarterTurn * Qty.sign (b - a)
      theta1 = Qty.interpolateFrom a b (Range.minValue t) + rotation
      theta2 = Qty.interpolateFrom a b (Range.maxValue t) + rotation
   in VectorBox2d.polar (Range.constant 1.0) (Range.from theta1 theta2)
tangentDirectionBounds t (Internal.Curve _ tolerance first second)
  | Range.includes 0.0 t && VectorCurve2d.evaluateAt 0.0 first ~= Vector2d.zero =
      VectorBox2d.normalize (VectorCurve2d.segmentBounds t second)
  | Range.includes 1.0 t && VectorCurve2d.evaluateAt 1.0 first ~= Vector2d.zero =
      -(VectorBox2d.normalize (VectorCurve2d.segmentBounds t second))
  | otherwise = VectorBox2d.normalize (VectorCurve2d.segmentBounds t first)
 where
  ?tolerance = tolerance

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
  VectorCurve2d.roots (point - curve)
    |> Result.withDefault [] -- Shouldn't happen, since curves are enforced to be non-degenerate

overlappingSegments
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> List (Domain, Domain, Sign)
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.successive
      ( \(u1, v1) (u2, v2) ->
          ( Range.from u1 u2
          , Range.from v1 v2
          , if compare u1 u2 == compare v1 v2 then Positive else Negative
          )
      )
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> (Domain, Domain, Sign)
  -> Bool
isOverlappingSegment curve1 curve2 (domain1, _, _) =
  let segmentStartPoint = evaluateAt (Range.minValue domain1) curve1
      curve1TestPoints = Domain.sample (pointOn curve1) domain1
      segment1IsNondegenerate = List.any (!= segmentStartPoint) curve1TestPoints
      segment1LiesOnSegment2 = List.all (\p1 -> passesThrough p1 curve2) curve1TestPoints
   in segment1IsNondegenerate && segment1LiesOnSegment2

data IntersectionError
  = CurvesOverlap (List (Domain, Domain, Sign))
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
  Bisection.Tree (Segment coordinateSystem)

findIntersections
  :: Tolerance units
  => Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> List (Float, Float)
  -> Result IntersectionError (List Intersection)
findIntersections curve1 curve2 endpointParameterValues = do
  let derivatives1 = Derivatives.ofCurve curve1
  let derivatives2 = Derivatives.ofCurve curve2
  let searchTree1 = Bisection.tree (Segment.init derivatives1)
  let searchTree2 = Bisection.tree (Segment.init derivatives2)
  endpointResults <-
    findEndpointIntersections derivatives1 derivatives2 endpointParameterValues searchTree1 searchTree2 ([], [])
  let (allIntersections, _) =
        endpointResults
          |> findTangentIntersections derivatives1 derivatives2 searchTree1 searchTree2
          |> findCrossingIntersections derivatives1 derivatives2 searchTree1 searchTree2
  Ok (List.sort allIntersections)

findEndpointIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> List (Float, Float)
  -> SearchTree (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> Result IntersectionError (List Intersection, List (Domain, Domain))
findEndpointIntersections _ _ [] _ _ accumulated = Ok accumulated
findEndpointIntersections derivatives1 derivatives2 (uv : rest) searchTree1 searchTree2 accumulated = do
  updated <- findEndpointIntersection derivatives1 derivatives2 uv searchTree1 searchTree2 accumulated
  findEndpointIntersections derivatives1 derivatives2 rest searchTree1 searchTree2 updated

findEndpointIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> (Float, Float)
  -> SearchTree (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> Result IntersectionError (List Intersection, List (Domain, Domain))
findEndpointIntersection derivatives1 derivatives2 uv searchTree1 searchTree2 accumulated = do
  intersectionType <-
    Derivatives.classify uv derivatives1 derivatives2
      |> Result.mapError (\Intersection.TangentIntersectionAtDegeneratePoint -> TangentIntersectionAtDegeneratePoint)
  let (intersectionKind, sign) = intersectionType
  let (u0, v0) = uv
  Ok $
    Bisection.solve2
      (Segment.isEndpointIntersectionCandidate uv)
      (Segment.endpointIntersectionResolved intersectionType)
      (\_ _ _ _ _ -> Just (Intersection u0 v0 intersectionKind sign))
      searchTree1
      searchTree2
      accumulated

findTangentIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> SearchTree (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> (List Intersection, List (Domain, Domain))
findTangentIntersections derivatives1 derivatives2 =
  Bisection.solve2
    Segment.isTangentIntersectionCandidate
    Segment.tangentIntersectionSign
    (Segment.findTangentIntersection derivatives1 derivatives2)

findCrossingIntersections
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> SearchTree (space @ units)
  -> SearchTree (space @ units)
  -> (List Intersection, List (Domain, Domain))
  -> (List Intersection, List (Domain, Domain))
findCrossingIntersections derivatives1 derivatives2 =
  Bisection.solve2
    Segment.isCrossingIntersectionCandidate
    Segment.crossingIntersectionSign
    (Segment.findCrossingIntersection derivatives1 derivatives2)

-- classify
--   :: Tolerance units
--   => Point2d (space @ units)
--   -> Curve2d (space @ units)
--   -> Sign
-- classify point curve =
