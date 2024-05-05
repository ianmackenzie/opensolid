module Curve2d
  ( Curve2d
  , DegenerateCurve (DegenerateCurve)
  , Intersection
  , IntersectionError (..)
  , Interface (..)
  , module Curve2d.Patterns
  , TransformBy (TransformBy)
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
  , transformBy
  , curvature
  , curvature'
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import Bisection qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Internal (DegenerateCurve (DegenerateCurve), Interface (..))
import Curve2d.Internal qualified as Internal
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.Patterns
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import DirectionCurve2d (DirectionCurve2d)
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Result qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

wrap :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
wrap = Internal.Curve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint = Internal.endPoint

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt = Internal.evaluateAt

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = evaluateAt t curve

segmentBounds :: Range Unitless -> Curve2d (space @ units) -> Bounds2d (space @ units)
segmentBounds = Internal.segmentBounds

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative = Internal.derivative

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse = Internal.reverse

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds = Internal.bounds

tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result DegenerateCurve (DirectionCurve2d space)
tangentDirection curve =
  VectorCurve2d.direction (derivative curve) ?? Error DegenerateCurve

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
    VectorCurve2d.Zeros parameterValues -> parameterValues
    -- Shouldn't happen, since curves are enforced to be non-degenerate
    VectorCurve2d.ZeroEverywhere -> []

overlappingSegments ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float) ->
  List (Range Unitless, Range Unitless, Sign)
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
  (Range Unitless, Range Unitless, Sign) ->
  Bool
isOverlappingSegment curve1 curve2 (domain1, _, _) = do
  let segmentStartPoint = evaluateAt (Range.minValue domain1) curve1
  let curve1TestPoints = List.map (pointOn curve1) (Range.samples domain1)
  let segment1IsNondegenerate = List.any (!= segmentStartPoint) curve1TestPoints
  let segment1LiesOnSegment2 = List.all (^ curve2) curve1TestPoints
  segment1IsNondegenerate && segment1LiesOnSegment2

data IntersectionError
  = CurvesOverlap (List (Range Unitless, Range Unitless, Sign))
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, Error)

findEndpointParameterValues ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float)
findEndpointParameterValues curve1 curve2 =
  List.sortAndDeduplicate $
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
  Ok (List.sort allIntersections)

findEndpointIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  List (Float, Float) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List Intersection, List (Range Unitless, Range Unitless)) ->
  Result IntersectionError (List Intersection, List (Range Unitless, Range Unitless))
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
  (List Intersection, List (Range Unitless, Range Unitless)) ->
  Result IntersectionError (List Intersection, List (Range Unitless, Range Unitless))
findEndpointIntersection derivatives1 derivatives2 t1t2 searchTree1 searchTree2 accumulated = Result.do
  let intersectionType = Derivatives.classify t1t2 derivatives1 derivatives2
  let (kind, sign) = intersectionType
  let (t1, t2) = t1t2
  Ok $
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
  (List Intersection, List (Range Unitless, Range Unitless)) ->
  (List Intersection, List (Range Unitless, Range Unitless))
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
  (List Intersection, List (Range Unitless, Range Unitless)) ->
  (List Intersection, List (Range Unitless, Range Unitless))
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

transformBy ::
  Transform2d a (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
transformBy = Internal.transformBy

curvature' :: Tolerance units => Curve2d (space @ units) -> Result DegenerateCurve (Curve1d (Unitless :/: units))
curvature' curve = Result.do
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  tangent <- tangentDirection curve
  Ok ((tangent >< secondDerivative) !/!. (firstDerivative .<>. firstDerivative))

curvature ::
  (Tolerance units1, Units.Quotient Unitless units1 units2) =>
  Curve2d (space @ units1) ->
  Result DegenerateCurve (Curve1d units2)
curvature curve = Result.map Units.specialize (curvature' curve)

data TransformBy curve coordinateSystem where
  TransformBy ::
    Interface curve (space @ units) =>
    Transform2d a (space @ units) ->
    curve ->
    TransformBy curve (space @ units)

deriving instance Show (TransformBy curve coordinateSystem)

instance Interface (TransformBy curve (space @ units)) (space @ units) where
  startPointImpl (TransformBy transform curve) = Point2d.transformBy transform (startPointImpl curve)
  endPointImpl (TransformBy transform curve) = Point2d.transformBy transform (endPointImpl curve)
  evaluateAtImpl t (TransformBy transform curve) = Point2d.transformBy transform (evaluateAtImpl t curve)
  segmentBoundsImpl t (TransformBy transform curve) = Bounds2d.transformBy transform (segmentBoundsImpl t curve)
  derivativeImpl (TransformBy transform curve) = VectorCurve2d.transformBy transform (derivativeImpl curve)
  reverseImpl (TransformBy transform curve) = TransformBy transform (reverseImpl curve)
  boundsImpl (TransformBy transform curve) = Bounds2d.transformBy transform (boundsImpl curve)
  transformByImpl transform (TransformBy existing curve) =
    Curve2d.wrap $
      TransformBy (Transform2d.toAffine existing >> Transform2d.toAffine transform) curve
