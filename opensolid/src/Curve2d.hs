module Curve2d
  ( Curve2d
  , pattern Point
  , pattern Line
  , pattern Arc
  , HasDegeneracy (HasDegeneracy)
  , Interface (..)
  , TransformBy (TransformBy)
  , new
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , tangentDirection
  , reverse
  , bounds
  , Intersections (..)
  , IntersectionPoint
  , OverlappingSegment
  , intersections
  , findPoint
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

import {-# SOURCE #-} Arc2d (Arc2d)
import {-# SOURCE #-} Arc2d qualified
import Axis2d (Axis2d)
import Axis2d qualified
import {-# SOURCE #-} BezierCurve2d qualified
import Bisection qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.FindPoint qualified as FindPoint
import Curve2d.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import Curve2d.IntersectionPoint qualified as IntersectionPoint
import Curve2d.Intersections qualified as Intersections
import Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import Curve2d.OverlappingSegment qualified as OverlappingSegment
import Curve2d.Segment (Segment)
import Curve2d.Segment qualified as Segment
import Direction2d (Direction2d)
import DirectionCurve2d (DirectionCurve2d)
import Error qualified
import Frame2d (Frame2d)
import Frame2d qualified
import {-# SOURCE #-} Line2d (Line2d)
import {-# SOURCE #-} Line2d qualified
import List qualified
import Maybe qualified
import OpenSolid
import Parameter qualified
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Result qualified
import Stream qualified
import Text qualified
import Tolerance qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified
import VectorCurve2d.Zeros qualified
import Prelude qualified

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface curve (space @ units) =>
    curve ->
    Curve2d (space @ units)
  Coerce ::
    Curve2d (space @ units1) ->
    Curve2d (space @ units2)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Curve2d (local @ units) ->
    Curve2d (global @ units)
  Addition ::
    Curve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    Curve2d (space @ units)
  Subtraction ::
    Curve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    Curve2d (space @ units)

deriving instance Show (Curve2d (space @ units))

instance HasUnits (Curve2d (space @ units)) where
  type Units (Curve2d (space @ units)) = units
  type Erase (Curve2d (space @ units)) = Curve2d (space @ Unitless)

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2d (space1 @ unitsA)) (Curve2d (space2 @ unitsB))
  where
  coerce (Coerce c) = Coerce c
  coerce c = Coerce c

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Curve2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  curve ^ point = VectorCurve2d.hasZero (curve - point)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Point2d (space1 @ units1)) (Curve2d (space2 @ units2)) units1
  where
  point ^ curve = curve ^ point

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Curve2d (space2 @ units2)) units1
  where
  curve1 ~= curve2 = List.allTrue [pointOn curve1 t ~= pointOn curve2 t | t <- Parameter.samples]

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  curve ~= point = List.allTrue [pointOn curve t ~= point | t <- Parameter.samples]

pattern Point :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units)
pattern Point point <- (asPoint -> Just point)

pattern Line :: Tolerance units => Line2d (space @ units) -> Curve2d (space @ units)
pattern Line line <- (asLine -> Just line)

pattern Arc :: Tolerance units => Arc2d (space @ units) -> Curve2d (space @ units)
pattern Arc arc <- (asArc -> Just arc)

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Error.Message)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  pointOnImpl :: curve -> Float -> Point2d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds2d coordinateSystem
  transformByImpl :: Transform2d tag coordinateSystem -> curve -> Curve2d coordinateSystem

  asLineImpl :: Tolerance (Units coordinateSystem) => curve -> Maybe (Line2d coordinateSystem)
  asLineImpl _ = Nothing
  asArcImpl :: Tolerance (Units coordinateSystem) => curve -> Maybe (Arc2d coordinateSystem)
  asArcImpl _ = Nothing

instance Interface (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds
  transformByImpl = transformBy
  asLineImpl = asLine
  asArcImpl = asArc

instance Interface (Point2d (space @ units)) (space @ units) where
  startPointImpl = identity
  endPointImpl = identity
  pointOnImpl point _ = point
  segmentBoundsImpl point _ = Bounds2d.constant point
  derivativeImpl _ = VectorCurve2d.zero
  reverseImpl = identity
  boundsImpl = Bounds2d.constant
  transformByImpl transform point = new (Point2d.transformBy transform point)

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

deriving instance Show (PointCurveDifference (space @ units))

instance VectorCurve2d.Interface (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - pointOn curve t
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds curve t
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)
  transformByImpl transform (PointCurveDifference point curve) =
    VectorCurve2d.new $
      PointCurveDifference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Interface states that the units of the transform
        -- do *not* have to match the units of the vector curve,
        -- because vectors and vector curves ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Point2d or Curve2d,
        -- but in this case it's safe since any translation will cancel out
        -- when the point and curve are subtracted from each other.
        (Point2d.transformBy (Units.coerce transform) point)
        (transformBy (Units.coerce transform) curve)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  c + VectorCurve2d.Constant v | v == Vector2d.zero = c
  c + v = Addition c v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  c - VectorCurve2d.Constant v | v == Vector2d.zero = c
  c - v = Subtraction c v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Curve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  point - curve = VectorCurve2d.new (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem)
  = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

deriving instance Show (CurvePointDifference (space @ units))

instance VectorCurve2d.Interface (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = pointOn curve t - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds curve t - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve
  transformByImpl transform (CurvePointDifference curve point) =
    VectorCurve2d.new $
      CurvePointDifference
        -- Note the same slight hack here as described in PointCurveDifference above
        (transformBy (Units.coerce transform) curve)
        (Point2d.transformBy (Units.coerce transform) point)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve - point = VectorCurve2d.new (CurvePointDifference curve point)

new :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
new = Curve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint curve = case curve of
  Curve c -> startPointImpl c
  Coerce c -> Units.coerce (startPoint c)
  PlaceIn frame c -> Point2d.placeIn frame (startPoint c)
  Addition c v -> startPoint c + VectorCurve2d.evaluateAt 0.0 v
  Subtraction c v -> startPoint c - VectorCurve2d.evaluateAt 0.0 v

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint curve = case curve of
  Curve c -> endPointImpl c
  Coerce c -> Units.coerce (endPoint c)
  PlaceIn frame c -> Point2d.placeIn frame (endPoint c)
  Addition c v -> endPoint c + VectorCurve2d.evaluateAt 1.0 v
  Subtraction c v -> endPoint c - VectorCurve2d.evaluateAt 1.0 v

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn curve t = case curve of
  Curve c -> pointOnImpl c t
  Coerce c -> Units.coerce (pointOn c t)
  PlaceIn frame c -> Point2d.placeIn frame (pointOn c t)
  Addition c v -> pointOn c t + VectorCurve2d.evaluateAt t v
  Subtraction c v -> pointOn c t - VectorCurve2d.evaluateAt t v

segmentBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
segmentBounds curve t = case curve of
  Curve c -> segmentBoundsImpl c t
  Coerce c -> Units.coerce (segmentBounds c t)
  PlaceIn frame c -> Bounds2d.placeIn frame (segmentBounds c t)
  Addition c v -> segmentBounds c t + VectorCurve2d.segmentBounds t v
  Subtraction c v -> segmentBounds c t - VectorCurve2d.segmentBounds t v

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve = case curve of
  Curve c -> derivativeImpl c
  Coerce c -> Units.coerce (derivative c)
  PlaceIn frame c -> VectorCurve2d.placeIn frame (derivative c)
  Addition c v -> derivative c + VectorCurve2d.derivative v
  Subtraction c v -> derivative c - VectorCurve2d.derivative v

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse curve = case curve of
  Curve c -> Curve (reverseImpl c)
  Coerce c -> Units.coerce (reverse c)
  PlaceIn frame c -> PlaceIn frame (reverse c)
  Addition c v -> reverse c + VectorCurve2d.reverse v
  Subtraction c v -> reverse c - VectorCurve2d.reverse v

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds curve = case curve of
  Curve c -> boundsImpl c
  Coerce c -> Units.coerce (bounds c)
  PlaceIn frame c -> Bounds2d.placeIn frame (bounds c)
  Addition c v -> bounds c + VectorCurve2d.segmentBounds Range.unit v
  Subtraction c v -> bounds c - VectorCurve2d.segmentBounds Range.unit v

asPoint :: Tolerance units => Curve2d (space @ units) -> Maybe (Point2d (space @ units))
asPoint curve = do
  let testPoint = pointOn curve 0.5
  let sampledPoints = List.map (pointOn curve) Parameter.samples
  if List.allSatisfy (~= testPoint) sampledPoints then Just testPoint else Nothing

asLine :: Tolerance units => Curve2d (space @ units) -> Maybe (Line2d (space @ units))
asLine curve = case curve of
  Curve c -> asLineImpl c
  Coerce c -> Maybe.map Units.coerce (Tolerance.using (Units.coerce ?tolerance) (asLine c))
  PlaceIn frame c -> Maybe.map (Line2d.placeIn frame) (asLine c)
  Addition{} -> Nothing
  Subtraction{} -> Nothing

asArc :: Tolerance units => Curve2d (space @ units) -> Maybe (Arc2d (space @ units))
asArc curve = case curve of
  Curve c -> asArcImpl c
  Coerce c -> Maybe.map Units.coerce (Tolerance.using (Units.coerce ?tolerance) (asArc c))
  PlaceIn frame c -> Maybe.map (Arc2d.placeIn frame) (asArc c)
  Addition{} -> Nothing
  Subtraction{} -> Nothing

tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (DirectionCurve2d space)
tangentDirection curve =
  case VectorCurve2d.direction (derivative curve) of
    Success directionCurve -> Success directionCurve
    Failure VectorCurve2d.HasZero -> Failure HasDegeneracy

signedDistanceAlong :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve1d units
signedDistanceAlong axis curve =
  (curve - Axis2d.originPoint axis) <> Axis2d.direction axis

xCoordinate :: Curve2d (space @ units) -> Curve1d units
xCoordinate = signedDistanceAlong Axis2d.x

yCoordinate :: Curve2d (space @ units) -> Curve1d units
yCoordinate = signedDistanceAlong Axis2d.y

findPoint ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Result FindPoint.Error (List Float)
findPoint point curve =
  case VectorCurve2d.zeros (point - curve) of
    Failure VectorCurve2d.Zeros.ZeroEverywhere -> Failure FindPoint.CurveIsCoincidentWithPoint
    Failure VectorCurve2d.Zeros.HigherOrderZero -> Failure FindPoint.HigherOrderSolution
    Success parameterValues -> Success parameterValues

overlappingSegments ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float) ->
  List OverlappingSegment
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.successive
      ( \(t1Start, t2Start) (t1End, t2End) ->
          OverlappingSegment
            (Range.from t1Start t1End)
            (Range.from t2Start t2End)
            (if (t1Start < t1End) == (t2Start < t2End) then Positive else Negative)
      )
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  OverlappingSegment ->
  Bool
isOverlappingSegment curve1 curve2 (OverlappingSegment{t1}) = do
  let segmentStartPoint = pointOn curve1 (Range.minValue t1)
  let curve1TestPoints = List.map (pointOn curve1) (Range.samples t1)
  let segment1IsNondegenerate = List.anySatisfy (!= segmentStartPoint) curve1TestPoints
  let segment1LiesOnSegment2 = List.allSatisfy (^ curve2) curve1TestPoints
  segment1IsNondegenerate && segment1LiesOnSegment2

findEndpointRoots ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Intersections.Error ->
  Result Intersections.Error (List Float)
findEndpointRoots endpoint curve curveIsPointError =
  case findPoint endpoint curve of
    Success parameterValues -> Success parameterValues
    Failure FindPoint.HigherOrderSolution -> Failure Intersections.HigherOrderIntersection
    Failure FindPoint.CurveIsCoincidentWithPoint -> Failure curveIsPointError

findEndpointParameterValues ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Intersections.Error (List (Float, Float))
findEndpointParameterValues curve1 curve2 = Result.do
  start1Roots <- findEndpointRoots (startPoint curve1) curve2 Intersections.SecondCurveIsPoint
  end1Roots <- findEndpointRoots (endPoint curve1) curve2 Intersections.SecondCurveIsPoint
  start2Roots <- findEndpointRoots (startPoint curve2) curve1 Intersections.FirstCurveIsPoint
  end2Roots <- findEndpointRoots (endPoint curve2) curve1 Intersections.FirstCurveIsPoint
  Success $
    List.sortAndDeduplicate $
      List.concat $
        [ List.map (0.0,) start1Roots
        , List.map (1.0,) end1Roots
        , List.map (,0.0) start2Roots
        , List.map (,1.0) end2Roots
        ]

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)
  deriving (Show)

intersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Intersections.Error (Maybe Intersections)
intersections curve1 curve2 = Result.do
  endpointParameterValues <- findEndpointParameterValues curve1 curve2
  case overlappingSegments curve1 curve2 endpointParameterValues of
    [] -> Result.do
      intersectionPoints <- findIntersectionPoints curve1 curve2 endpointParameterValues
      case intersectionPoints of
        [] -> Success Nothing
        NonEmpty points -> Success (Just (IntersectionPoints points))
    NonEmpty segments -> Success (Just (OverlappingSegments segments))

type SearchTree (coordinateSystem :: CoordinateSystem) =
  Bisection.Tree (Segment coordinateSystem)

findIntersectionPoints ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  List (Float, Float) ->
  Result Intersections.Error (List IntersectionPoint)
findIntersectionPoints curve1 curve2 endpointParameterValues = Result.do
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
  Success (List.sort allIntersections)

findEndpointIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  List (Float, Float) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List IntersectionPoint, List (Range Unitless, Range Unitless)) ->
  Result Intersections.Error (List IntersectionPoint, List (Range Unitless, Range Unitless))
findEndpointIntersections _ _ [] _ _ accumulated = Success accumulated
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
  (List IntersectionPoint, List (Range Unitless, Range Unitless)) ->
  Result Intersections.Error (List IntersectionPoint, List (Range Unitless, Range Unitless))
findEndpointIntersection derivatives1 derivatives2 t1t2 searchTree1 searchTree2 accumulated = Result.do
  let intersectionType = Derivatives.classify t1t2 derivatives1 derivatives2
  let (kind, sign) = intersectionType
  let (t1, t2) = t1t2
  Success $
    Bisection.solve2
      (Segment.isEndpointIntersectionCandidate t1t2)
      (Segment.endpointIntersectionResolved intersectionType)
      (\_ _ _ _ _ -> Just (IntersectionPoint{t1, t2, kind, sign}))
      searchTree1
      searchTree2
      accumulated

findTangentIntersections ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  SearchTree (space @ units) ->
  SearchTree (space @ units) ->
  (List IntersectionPoint, List (Range Unitless, Range Unitless)) ->
  (List IntersectionPoint, List (Range Unitless, Range Unitless))
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
  (List IntersectionPoint, List (Range Unitless, Range Unitless)) ->
  (List IntersectionPoint, List (Range Unitless, Range Unitless))
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
  PlaceIn frame localCurve -> PlaceIn (Frame2d.placeIn globalFrame frame) localCurve
  _ -> PlaceIn globalFrame curve

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Curve2d (global @ units) ->
  Curve2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

transformBy :: Transform2d tag (space @ units) -> Curve2d (space @ units) -> Curve2d (space @ units)
transformBy transform curve = case curve of
  Curve c -> Curve (transformByImpl transform c)
  Coerce c -> Units.coerce (transformBy (Units.coerce transform) c)
  PlaceIn frame c -> PlaceIn frame (transformBy (Transform2d.relativeTo frame transform) c)
  Addition c v -> transformBy transform c + VectorCurve2d.transformBy transform v
  Subtraction c v -> transformBy transform c - VectorCurve2d.transformBy transform v

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
  Result HasDegeneracy (Curve1d (Unitless :/: units))
curvature' curve = Result.do
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  tangent <- tangentDirection curve
  Success ((tangent >< secondDerivative) !/!. (firstDerivative .<>. firstDerivative))

curvature ::
  (Tolerance units1, Units.Quotient Unitless units1 units2) =>
  Curve2d (space @ units1) ->
  Result HasDegeneracy (Curve1d units2)
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
    Curve2d.new $
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
        VectorCurve2d.new $
          SyntheticDerivative
            (nthDerivative n (baseCurve (continuity + n)))
            (curveDerivative (n + 1))
  new (Synthetic (baseCurve continuity) (curveDerivative 1))

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
    Curve2d.new $
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
    VectorCurve2d.new $
      SyntheticDerivative
        (VectorCurve2d.transformBy transform current)
        (VectorCurve2d.transformBy transform next)
