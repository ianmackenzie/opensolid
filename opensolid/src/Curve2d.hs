module Curve2d
  ( Curve2d
  , DegenerateCurve (DegenerateCurve)
  , Intersection
  , IntersectionError (..)
  , Interface (..)
  , TransformBy (TransformBy)
  , wrap
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , tangentDirection
  , reverse
  , bounds
  , intersections
  , HigherOrderZero (HigherOrderZero)
  , find
  , signedDistanceAlong
  , xCoordinate
  , yCoordinate
  , placeIn
  , relativeTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  , mirrorAcrossOwn
  , scaleAboutOwn
  , scaleAlongOwn
  , curvature
  , curvature'
  , removeStartDegeneracy
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import {-# SOURCE #-} BezierCurve2d qualified
import Bisection qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Internal (Interface (..))
import Curve2d.Internal qualified as Internal
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import Direction2d (Direction2d)
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
import Stream qualified
import Text qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d)
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified
import Prelude qualified

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, Error)

wrap :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
wrap = Internal.Curve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint = Internal.endPoint

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn = Internal.pointOn

segmentBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
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

data HigherOrderZero = HigherOrderZero deriving (Eq, Show, Error)

find ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Result HigherOrderZero (List Float)
find point curve =
  case VectorCurve2d.zeros (point - curve) of
    Error VectorCurve2d.HigherOrderZero -> Error HigherOrderZero
    Ok (VectorCurve2d.Zeros parameterValues) -> Ok parameterValues
    -- Shouldn't happen, since curves are enforced to be non-degenerate
    Ok VectorCurve2d.ZeroEverywhere -> Ok []

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
  let segmentStartPoint = pointOn curve1 (Range.minValue domain1)
  let curve1TestPoints = List.map (pointOn curve1) (Range.samples domain1)
  let segment1IsNondegenerate = List.any (!= segmentStartPoint) curve1TestPoints
  let segment1LiesOnSegment2 = List.all (^ curve2) curve1TestPoints
  segment1IsNondegenerate && segment1LiesOnSegment2

data IntersectionError
  = CurvesOverlap (List (Range Unitless, Range Unitless, Sign))
  | TangentIntersectionAtDegeneratePoint
  | HigherOrderIntersection
  deriving (Eq, Show, Error)

findEndpointParameterValues ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result HigherOrderZero (List (Float, Float))
findEndpointParameterValues curve1 curve2 = Result.do
  start1Roots <- find (startPoint curve1) curve2
  end1Roots <- find (endPoint curve1) curve2
  start2Roots <- find (startPoint curve2) curve1
  end2Roots <- find (endPoint curve2) curve1
  Ok $
    List.sortAndDeduplicate $
      List.concat $
        [ List.map (0.0,) start1Roots
        , List.map (1.0,) end1Roots
        , List.map (,0.0) start2Roots
        , List.map (,1.0) end2Roots
        ]

intersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result IntersectionError (List Intersection)
intersections curve1 curve2 = Result.do
  endpointParameterValues <-
    findEndpointParameterValues curve1 curve2 ?? Error HigherOrderIntersection
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
placeIn globalFrame curve = case curve of
  Internal.PlaceIn frame localCurve -> Internal.PlaceIn (Frame2d.placeIn globalFrame frame) localCurve
  _ -> Internal.PlaceIn globalFrame curve

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Curve2d (global @ units) ->
  Curve2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
transformBy = Internal.transformBy

translateBy ::
  Vector2d (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d (space @ units) ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2d (space @ units) ->
  Angle ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout ::
  Point2d (space @ units) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong ::
  Axis2d (space @ units) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAlong = Transform2d.scaleAlongImpl transformBy

translateByOwn ::
  ( Curve2d (space @ units) ->
    Vector2d (space @ units)
  ) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateByOwn = Transform2d.translateByOwnImpl transformBy

translateInOwn ::
  ( Curve2d (space @ units) ->
    Direction2d space
  ) ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateInOwn = Transform2d.translateInOwnImpl transformBy

translateAlongOwn ::
  ( Curve2d (space @ units) ->
    Axis2d (space @ units)
  ) ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateAlongOwn = Transform2d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  ( Curve2d (space @ units) ->
    Point2d (space @ units)
  ) ->
  Angle ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
rotateAroundOwn = Transform2d.rotateAroundOwnImpl transformBy

mirrorAcrossOwn ::
  ( Curve2d (space @ units) ->
    Axis2d (space @ units)
  ) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
mirrorAcrossOwn = Transform2d.mirrorAcrossOwnImpl transformBy

scaleAboutOwn ::
  ( Curve2d (space @ units) ->
    Point2d (space @ units)
  ) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAboutOwn = Transform2d.scaleAboutOwnImpl transformBy

scaleAlongOwn ::
  ( Curve2d (space @ units) ->
    Axis2d (space @ units)
  ) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAlongOwn = Transform2d.scaleAlongOwnImpl transformBy

curvature' ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result DegenerateCurve (Curve1d (Unitless :/: units))
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
    Transform2d tag (space @ units) ->
    curve ->
    TransformBy curve (space @ units)

deriving instance Show (TransformBy curve coordinateSystem)

instance Interface (TransformBy curve (space @ units)) (space @ units) where
  startPointImpl (TransformBy transform curve) =
    Point2d.transformBy transform (startPointImpl curve)
  endPointImpl (TransformBy transform curve) =
    Point2d.transformBy transform (endPointImpl curve)
  pointOnImpl (TransformBy transform curve) t =
    Point2d.transformBy transform (pointOnImpl curve t)
  segmentBoundsImpl (TransformBy transform curve) t =
    Bounds2d.transformBy transform (segmentBoundsImpl curve t)
  derivativeImpl (TransformBy transform curve) =
    VectorCurve2d.transformBy transform (derivativeImpl curve)
  reverseImpl (TransformBy transform curve) =
    TransformBy transform (reverseImpl curve)
  boundsImpl (TransformBy transform curve) =
    Bounds2d.transformBy transform (boundsImpl curve)
  transformByImpl transform (TransformBy existing curve) =
    Curve2d.wrap $
      TransformBy (Transform2d.toAffine existing >> Transform2d.toAffine transform) curve

removeStartDegeneracy ::
  Int ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
removeStartDegeneracy continuity startCondition curve = Result.do
  let curveDerivatives = Stream.iterate (derivative curve) VectorCurve2d.derivative
  let endDerivativeValues = Stream.map (VectorCurve2d.evaluateAt 1.0) curveDerivatives
  let endCondition endDegree = (endPoint curve, Stream.take endDegree endDerivativeValues)
  let baseCurve endDegree = BezierCurve2d.hermite startCondition (endCondition endDegree)
  let curveDerivative n =
        VectorCurve2d.wrap $
          SyntheticDerivative
            (nthDerivative n (baseCurve (continuity + n)))
            (curveDerivative (n + 1))
  wrap (Synthetic (baseCurve continuity) (curveDerivative 1))

nthDerivative :: Int -> Curve2d (space @ units) -> VectorCurve2d (space @ units)
nthDerivative 0 _ = internalError "nthDerivative should always be called with n >= 1"
nthDerivative 1 curve = derivative curve
nthDerivative n curve = VectorCurve2d.derivative (nthDerivative (n - 1) curve)

data Synthetic coordinateSystem where
  Synthetic ::
    Curve2d (space @ units) ->
    ~(VectorCurve2d (space @ units)) ->
    Synthetic (space @ units)

instance Show (Synthetic (space @ units)) where
  show _ = Text.unpack "<Synthetic>"

instance Interface (Synthetic (space @ units)) (space @ units) where
  startPointImpl (Synthetic curve _) = Curve2d.startPoint curve
  endPointImpl (Synthetic curve _) = Curve2d.endPoint curve
  pointOnImpl (Synthetic curve _) t = Curve2d.pointOn curve t
  segmentBoundsImpl (Synthetic curve _) t = Curve2d.segmentBounds curve t
  boundsImpl (Synthetic curve _) = Curve2d.bounds curve
  reverseImpl (Synthetic curve curveDerivative) =
    Synthetic (Curve2d.reverse curve) (-(VectorCurve2d.reverse curveDerivative))
  derivativeImpl (Synthetic _ curveDerivative) = curveDerivative
  transformByImpl transform (Synthetic curve curveDerivative) =
    Curve2d.wrap $
      Synthetic
        (Curve2d.transformBy transform curve)
        (VectorCurve2d.transformBy transform curveDerivative)

data SyntheticDerivative coordinateSystem where
  SyntheticDerivative ::
    VectorCurve2d (space @ units) ->
    ~(VectorCurve2d (space @ units)) ->
    SyntheticDerivative (space @ units)

instance Show (SyntheticDerivative (space @ units)) where
  show _ = Text.unpack "<SyntheticDerivative>"

instance VectorCurve2d.Interface (SyntheticDerivative (space @ units)) (space @ units) where
  evaluateAtImpl t (SyntheticDerivative current _) = VectorCurve2d.evaluateAt t current
  segmentBoundsImpl t (SyntheticDerivative current _) = VectorCurve2d.segmentBounds t current
  derivativeImpl (SyntheticDerivative _ next) = next
  transformByImpl transform (SyntheticDerivative current next) =
    VectorCurve2d.wrap $
      SyntheticDerivative
        (VectorCurve2d.transformBy transform current)
        (VectorCurve2d.transformBy transform next)
