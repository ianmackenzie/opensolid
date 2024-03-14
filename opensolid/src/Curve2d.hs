module Curve2d
  ( Curve2d
  , DegenerateCurve (DegenerateCurve)
  , Intersection
  , IntersectionError (..)
  , Interface (..)
  , module Curve2d.Patterns
  , wrap
  , startPoint
  , endPoint
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , tangentDirection
  , reverse
  , bounds
  , intersections
  , find
  , signedDistanceAlong
  , xCoordinate
  , yCoordinate
  , placeIn
  , relativeTo
  , curvature
  )
where

import Angle qualified
import Axis2d (Axis2d)
import Axis2d qualified
import Bisection qualified
import Bounds2d (Bounds2d)
import Curve1d (Curve1d)
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Internal (Interface (..))
import Curve2d.Internal qualified as Internal
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.Patterns
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import DirectionCurve2d (DirectionCurve2d)
import DirectionCurve2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range qualified
import Result qualified
import T qualified
import Units qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, Error)

wrap ::
  ( Tolerance units
  , Interface curve (space @ units)
  ) =>
  curve ->
  Result DegenerateCurve (Curve2d (space @ units))
wrap curve =
  case VectorCurve2d.direction (derivativeImpl curve) of
    Ok tangentCurve -> Ok (Internal.Curve curve tangentCurve)
    Error VectorCurve2d.DegenerateCurve -> Error DegenerateCurve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint = Internal.endPoint

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt = Internal.evaluateAt

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = evaluateAt t curve

segmentBounds :: T.Bounds -> Curve2d (space @ units) -> Bounds2d (space @ units)
segmentBounds = Internal.segmentBounds

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative = Internal.derivative

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse = Internal.reverse

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds = Internal.bounds

tangentDirection :: Curve2d (space @ units) -> DirectionCurve2d space
tangentDirection (Internal.Line{direction}) = DirectionCurve2d.constant direction
tangentDirection (Internal.Arc{startAngle, endAngle}) =
  let tangentStartAngle = startAngle + Angle.quarterTurn
      tangentEndAngle = endAngle + Angle.quarterTurn
   in Qty.sign (endAngle - startAngle) * DirectionCurve2d.arc tangentStartAngle tangentEndAngle
tangentDirection (Internal.Curve _ tangent) = tangent
tangentDirection (Internal.PlaceIn frame curve) = DirectionCurve2d.placeIn frame (tangentDirection curve)

data CurveIsCoincidentWithPoint = CurveIsCoincidentWithPoint deriving (Eq, Show, Error)

signedDistanceAlong :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve1d units
signedDistanceAlong axis curve =
  (curve - Axis2d.originPoint axis) <> Axis2d.direction axis

xCoordinate :: Curve2d (space @ units) -> Curve1d units
xCoordinate = signedDistanceAlong Axis2d.x

yCoordinate :: Curve2d (space @ units) -> Curve1d units
yCoordinate = signedDistanceAlong Axis2d.y

find ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  List Float
find point curve =
  case VectorCurve2d.zeros (point - curve) of
    Ok parameterValues -> parameterValues
    -- Shouldn't happen, since curves are enforced to be non-degenerate
    Error VectorCurve2d.ZeroEverywhere -> []

overlappingSegments ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float) ->
  List (T.Bounds, T.Bounds, Sign)
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.successive
      ( \(t1Start, t2Start) (t1End, t2End) ->
          ( Range.from t1Start t1End
          , Range.from t2Start t2End
          , if (t1Start < t1End) == (t2Start < t2End) then Positive else Negative
          )
      )
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  (T.Bounds, T.Bounds, Sign) ->
  Bool
isOverlappingSegment curve1 curve2 (domain1, _, _) =
  let segmentStartPoint = evaluateAt (Range.minValue domain1) curve1
      curve1TestPoints = List.map (pointOn curve1) (Range.samples domain1)
      segment1IsNondegenerate = List.any (!= segmentStartPoint) curve1TestPoints
      segment1LiesOnSegment2 = List.all (^ curve2) curve1TestPoints
   in segment1IsNondegenerate && segment1LiesOnSegment2

data IntersectionError
  = CurvesOverlap (List (T.Bounds, T.Bounds, Sign))
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, Error)

findEndpointParameterValues ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float)
findEndpointParameterValues curve1 curve2 =
  List.sortAndDeduplicate <|
    List.concat
      [ List.map (0.0,) (find (startPoint curve1) curve2)
      , List.map (1.0,) (find (endPoint curve1) curve2)
      , List.map (,0.0) (find (startPoint curve2) curve1)
      , List.map (,1.0) (find (endPoint curve2) curve1)
      ]

intersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result IntersectionError (List Intersection)
intersections curve1 curve2 = Result.do
  let endpointParameterValues = findEndpointParameterValues curve1 curve2
  case overlappingSegments curve1 curve2 endpointParameterValues of
    [] -> findIntersections curve1 curve2 endpointParameterValues
    segments -> Error (CurvesOverlap segments)

type SearchTree (coordinateSystem :: CoordinateSystem) =
  Bisection.Tree (Segment coordinateSystem)

findIntersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float) ->
  Result IntersectionError (List Intersection)
findIntersections curve1 curve2 endpointParameterValues = Result.do
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
  return (List.sort allIntersections)

findEndpointIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  List (Float, Float) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List Intersection, List (T.Bounds, T.Bounds)) ->
  Result IntersectionError (List Intersection, List (T.Bounds, T.Bounds))
findEndpointIntersections _ _ [] _ _ accumulated = Ok accumulated
findEndpointIntersections derivatives1 derivatives2 (uv : rest) searchTree1 searchTree2 accumulated = Result.do
  updated <- findEndpointIntersection derivatives1 derivatives2 uv searchTree1 searchTree2 accumulated
  findEndpointIntersections derivatives1 derivatives2 rest searchTree1 searchTree2 updated

findEndpointIntersection ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  (Float, Float) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List Intersection, List (T.Bounds, T.Bounds)) ->
  Result IntersectionError (List Intersection, List (T.Bounds, T.Bounds))
findEndpointIntersection derivatives1 derivatives2 t1t2 searchTree1 searchTree2 accumulated = Result.do
  intersectionType <- Derivatives.classify t1t2 derivatives1 derivatives2 ?? TangentIntersectionAtDegeneratePoint
  let (kind, sign) = intersectionType
  let (t1, t2) = t1t2
  return <|
    Bisection.solve2
      (Segment.isEndpointIntersectionCandidate t1t2)
      (Segment.endpointIntersectionResolved intersectionType)
      (\_ _ _ _ _ -> Just (Intersection{t1, t2, kind, sign}))
      searchTree1
      searchTree2
      accumulated

findTangentIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List Intersection, List (T.Bounds, T.Bounds)) ->
  (List Intersection, List (T.Bounds, T.Bounds))
findTangentIntersections derivatives1 derivatives2 =
  Bisection.solve2
    Segment.isTangentIntersectionCandidate
    Segment.tangentIntersectionSign
    (Segment.findTangentIntersection derivatives1 derivatives2)

findCrossingIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List Intersection, List (T.Bounds, T.Bounds)) ->
  (List Intersection, List (T.Bounds, T.Bounds))
findCrossingIntersections derivatives1 derivatives2 =
  Bisection.solve2
    Segment.isCrossingIntersectionCandidate
    Segment.crossingIntersectionSign
    (Segment.findCrossingIntersection derivatives1 derivatives2)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve2d (global @ units)
placeIn globalFrame (Internal.PlaceIn frame curve) = Internal.PlaceIn (Frame2d.placeIn globalFrame frame) curve
placeIn globalFrame curve = Internal.PlaceIn globalFrame curve

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Curve2d (global @ units) ->
  Curve2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

curvature :: Qty units -> Curve2d (space @ units) -> Curve1d Unitless
curvature referenceRadius curve = do
  let firstDerivative = Units.generalize (derivative curve)
  let tangent = tangentDirection curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  (tangent >< secondDerivative .* Units.generalize referenceRadius) / (firstDerivative .<> firstDerivative)
