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
import Bisection qualified
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Internal (IsCurve2d (..))
import Curve2d.Internal qualified as Internal
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import Direction2d (Direction2d)
import Domain (Domain)
import Domain qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units qualified
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
   in if Range.any (areBothZero firstDerivative secondDerivative) Domain.unit
        then Error DegenerateCurve
        else Ok (Internal.Curve curve firstDerivative secondDerivative)

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
boundingBox (Internal.Curve curve _ _) = boundingBoxImpl curve

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
