module OpenSolid.Curve2d
  ( Curve2d (Parametric)
  , pattern Point
  , HasDegeneracy (HasDegeneracy)
  , Interface (..)
  , TransformBy (TransformBy)
  , new
  , constant
  , xy
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
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
  , toPolyline
  , medialAxis
  , arcLengthParameterization
  , parameterizeByArcLength
  , piecewise
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Arithmetic qualified as Arithmetic
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import {-# SOURCE #-} OpenSolid.BezierCurve2d qualified as BezierCurve2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Composition
import OpenSolid.Curve1d (Curve1d)
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d.FindPoint qualified as FindPoint
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve2d.MedialAxis qualified as MedialAxis
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2d.OverlappingSegment qualified as OverlappingSegment
import OpenSolid.Debug qualified as Debug
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Domain2d (Domain2d)
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Result qualified as Result
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.Stream qualified as Stream
import OpenSolid.Surface1d.Function qualified as Surface1d.Function
import OpenSolid.Surface1d.Function.Zeros qualified as Surface1d.Function.Zeros
import OpenSolid.Surface2d.Function qualified
import {-# SOURCE #-} OpenSolid.Surface2d.Function qualified as Surface2d.Function
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve2d.Zeros qualified as VectorCurve2d.Zeros
import OpenSolid.VectorSurface2d.Function qualified as VectorSurface2d.Function
import Prelude qualified

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface curve coordinateSystem =>
    curve ->
    Curve2d coordinateSystem
  Parametric ::
    Expression Float (Point2d (space @ units)) ->
    Curve2d (space @ units)
  Coerce ::
    Curve2d (space @ units1) ->
    Curve2d (space @ units2)
  XY ::
    Curve1d units ->
    Curve1d units ->
    Curve2d (space @ units)
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

instance HasUnits (Curve2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2d (space1 @ unitsA)) (Curve2d (space2 @ unitsB))
  where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce c) = Coerce c
  coerce c = Coerce c

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Curve2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  curve ^ point = VectorCurve2d.hasZero (curve - point)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2d (space1 @ units1)) (Curve2d (space2 @ units2)) units1
  where
  point ^ curve = curve ^ point

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Curve2d (space2 @ units2)) units1
  where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  curve ~= point = List.allTrue [evaluate curve t ~= point | t <- Parameter.samples]

pattern Point :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units)
pattern Point point <- (asPoint -> Just point)

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Error.Message)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateImpl :: curve -> Float -> Point2d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds2d coordinateSystem
  transformByImpl :: Transform2d tag coordinateSystem -> curve -> Curve2d coordinateSystem

  startPointImpl curve = evaluateImpl curve 0.0
  endPointImpl curve = evaluateImpl curve 1.0
  boundsImpl curve = evaluateBoundsImpl curve Range.unit

instance Interface (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds
  transformByImpl = transformBy

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  Parametric lhs + VectorCurve2d.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Addition lhs rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  Parametric lhs - VectorCurve2d.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Subtraction lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (Curve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = VectorCurve2d.Parametric (lhs - rhs)
  lhs - rhs = VectorCurve2d.new (Arithmetic.Difference lhs rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  VectorCurve2d.Interface
    (Arithmetic.Difference (Curve2d (space1 @ units1)) (Curve2d (space2 @ units2)))
    (space1 @ units1)
  where
  evaluateImpl (Arithmetic.Difference curve1 curve2) tValue =
    evaluate curve1 tValue - evaluate curve2 tValue

  evaluateBoundsImpl (Arithmetic.Difference curve1 curve2) tRange =
    evaluateBounds curve1 tRange - evaluateBounds curve2 tRange

  derivativeImpl (Arithmetic.Difference curve1 curve2) =
    derivative curve1 - derivative curve2

  transformByImpl transform (Arithmetic.Difference curve1 curve2) =
    VectorCurve2d.new $
      Arithmetic.Difference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Interface states that the units of the transform
        -- do *not* have to match the units of the vector curve,
        -- because vectors and vector curves ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Curve2d,
        -- but in this case it's safe since any translation will cancel out
        -- when the two curves are subtracted from each other.
        (transformBy (Units.coerce transform) curve1)
        (transformBy (Units.coerce transform) curve2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve - point = curve - constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Curve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  point - curve = constant point - curve

instance Composition (Curve1d Unitless) (Curve2d (space @ units)) (Curve2d (space @ units)) where
  Parametric outer . Curve1d.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance Interface (Curve2d (space @ units) :.: Curve1d Unitless) (space @ units) where
  evaluateImpl (curve2d :.: curve1d) tRange =
    evaluate curve2d (Curve1d.evaluate curve1d tRange)

  evaluateBoundsImpl (curve2d :.: curve1d) tRange =
    evaluateBounds curve2d (Curve1d.evaluateBounds curve1d tRange)

  derivativeImpl (curve2d :.: curve1d) =
    (derivative curve2d . curve1d) * Curve1d.derivative curve1d

  reverseImpl (curve2d :.: curve1d) =
    curve2d :.: Curve1d.reverse curve1d

  transformByImpl transform (curve2d :.: curve1d) =
    new (transformBy transform curve2d . curve1d)

instance
  Composition
    (Surface1d.Function.Function Unitless)
    (Curve2d (space @ units))
    (Surface2d.Function.Function (space @ units))
  where
  Parametric outer . Surface1d.Function.Parametric inner = Surface2d.Function.Parametric (outer . inner)
  curve . function = Surface2d.Function.new (curve :.: function)

instance
  Surface2d.Function.Interface
    (Curve2d (space @ units) :.: Surface1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: function) uvPoint =
    evaluate curve (Surface1d.Function.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    evaluateBounds curve (Surface1d.Function.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (derivative curve . function) * Surface1d.Function.derivative parameter function

  transformByImpl transform (curve :.: function) =
    transformBy transform curve . function

instance
  Composition
    (Curve2d UvCoordinates)
    (Surface1d.Function.Function units)
    (Curve1d units)
  where
  Surface1d.Function.Parametric outer . Parametric inner = Curve1d.Parametric (outer . inner)
  outer . inner = Curve1d.new (outer :.: inner)

instance Curve1d.Interface (Surface1d.Function.Function units :.: Curve2d UvCoordinates) units where
  evaluateImpl (function :.: uvCurve) t =
    Surface1d.Function.evaluate function (evaluate uvCurve t)

  evaluateBoundsImpl (function :.: uvCurve) t =
    Surface1d.Function.evaluateBounds function (evaluateBounds uvCurve t)

  derivativeImpl (function :.: uvCurve) = do
    let fU = Surface1d.Function.derivative U function
    let fV = Surface1d.Function.derivative V function
    let uvT = derivative uvCurve
    let uT = VectorCurve2d.xComponent uvT
    let vT = VectorCurve2d.yComponent uvT
    fU . uvCurve * uT + fV . uvCurve * vT

new :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
new = Curve

constant :: Point2d (space @ units) -> Curve2d (space @ units)
constant = Parametric . Expression.constant

xy :: Curve1d units -> Curve1d units -> Curve2d (space @ units)
xy (Curve1d.Parametric x) (Curve1d.Parametric y) = Parametric (Expression.xy x y)
xy x y = XY x y

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint curve = case curve of
  Curve c -> startPointImpl c
  Parametric expresssion -> Expression.evaluate expresssion 0.0
  Coerce c -> Units.coerce (startPoint c)
  XY x y -> Point2d.xy (Curve1d.evaluate x 0.0) (Curve1d.evaluate y 0.0)
  PlaceIn frame c -> Point2d.placeIn frame (startPoint c)
  Addition c v -> startPoint c + VectorCurve2d.startValue v
  Subtraction c v -> startPoint c - VectorCurve2d.startValue v

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint curve = case curve of
  Curve c -> endPointImpl c
  Parametric expresssion -> Expression.evaluate expresssion 1.0
  Coerce c -> Units.coerce (endPoint c)
  XY x y -> Point2d.xy (Curve1d.evaluate x 1.0) (Curve1d.evaluate y 1.0)
  PlaceIn frame c -> Point2d.placeIn frame (endPoint c)
  Addition c v -> endPoint c + VectorCurve2d.endValue v
  Subtraction c v -> endPoint c - VectorCurve2d.endValue v

evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluate curve tValue = case curve of
  Curve c -> evaluateImpl c tValue
  Parametric expresssion -> Expression.evaluate expresssion tValue
  Coerce c -> Units.coerce (evaluate c tValue)
  XY x y -> Point2d.xy (Curve1d.evaluate x tValue) (Curve1d.evaluate y tValue)
  PlaceIn frame c -> Point2d.placeIn frame (evaluate c tValue)
  Addition c v -> evaluate c tValue + VectorCurve2d.evaluate v tValue
  Subtraction c v -> evaluate c tValue - VectorCurve2d.evaluate v tValue

evaluateBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
evaluateBounds curve tRange = case curve of
  Curve c -> evaluateBoundsImpl c tRange
  Parametric expresssion -> Expression.evaluateBounds expresssion tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  XY x y -> Bounds2d.xy (Curve1d.evaluateBounds x tRange) (Curve1d.evaluateBounds y tRange)
  PlaceIn frame c -> Bounds2d.placeIn frame (evaluateBounds c tRange)
  Addition c v -> evaluateBounds c tRange + VectorCurve2d.evaluateBounds v tRange
  Subtraction c v -> evaluateBounds c tRange - VectorCurve2d.evaluateBounds v tRange

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve = case curve of
  Curve c -> derivativeImpl c
  Parametric expresssion -> VectorCurve2d.Parametric (Expression.curveDerivative expresssion)
  XY x y -> VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)
  Coerce c -> Units.coerce (derivative c)
  PlaceIn frame c -> VectorCurve2d.placeIn frame (derivative c)
  Addition c v -> derivative c + VectorCurve2d.derivative v
  Subtraction c v -> derivative c - VectorCurve2d.derivative v

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse curve = case curve of
  Curve c -> Curve (reverseImpl c)
  Parametric expression -> Parametric (expression . Expression.r)
  XY x y -> XY (Curve1d.reverse x) (Curve1d.reverse y)
  Coerce c -> Units.coerce (reverse c)
  PlaceIn frame c -> PlaceIn frame (reverse c)
  Addition c v -> reverse c + VectorCurve2d.reverse v
  Subtraction c v -> reverse c - VectorCurve2d.reverse v

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds curve = case curve of
  Curve c -> boundsImpl c
  Parametric expression -> Expression.evaluateBounds expression Range.unit
  Coerce c -> Units.coerce (bounds c)
  XY x y -> Bounds2d.xy (Curve1d.evaluateBounds x Range.unit) (Curve1d.evaluateBounds y Range.unit)
  PlaceIn frame c -> Bounds2d.placeIn frame (bounds c)
  Addition c v -> bounds c + VectorCurve2d.evaluateBounds v Range.unit
  Subtraction c v -> bounds c - VectorCurve2d.evaluateBounds v Range.unit

asPoint :: Tolerance units => Curve2d (space @ units) -> Maybe (Point2d (space @ units))
asPoint curve = do
  let testPoint = evaluate curve 0.5
  let sampledPoints = List.map (evaluate curve) Parameter.samples
  if List.allSatisfy (~= testPoint) sampledPoints then Just testPoint else Nothing

tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (DirectionCurve2d space)
tangentDirection curve =
  case VectorCurve2d.direction (derivative curve) of
    Success directionCurve -> Success directionCurve
    Failure VectorCurve2d.HasZero -> Failure HasDegeneracy

signedDistanceAlong :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve1d units
signedDistanceAlong axis curve = (curve - Axis2d.originPoint axis) <> Axis2d.direction axis

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
  List UvPoint ->
  List OverlappingSegment
overlappingSegments curve1 curve2 endpointParameterValues =
  endpointParameterValues
    |> List.successive
      ( \(Point2d t1Start t2Start) (Point2d t1End t2End) ->
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
  let segmentStartPoint = evaluate curve1 (Range.lowerBound t1)
  let curve1TestPoints = List.map (evaluate curve1) (Range.samples t1)
  let segment1IsNondegenerate = List.anySatisfy (!= segmentStartPoint) curve1TestPoints
  let segment1LiesOnSegment2 = List.allSatisfy (^ curve2) curve1TestPoints
  segment1IsNondegenerate && segment1LiesOnSegment2

findEndpointZeros ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Intersections.Error ->
  Result Intersections.Error (List Float)
findEndpointZeros endpoint curve curveIsPointError =
  case findPoint endpoint curve of
    Success parameterValues -> Success parameterValues
    Failure FindPoint.HigherOrderSolution -> Failure Intersections.HigherOrderIntersection
    Failure FindPoint.CurveIsCoincidentWithPoint -> Failure curveIsPointError

findEndpointIntersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Intersections.Error (List UvPoint)
findEndpointIntersections curve1 curve2 = Result.do
  start1Zeros <- findEndpointZeros (startPoint curve1) curve2 Intersections.SecondCurveIsPoint
  end1Zeros <- findEndpointZeros (endPoint curve1) curve2 Intersections.SecondCurveIsPoint
  start2Zeros <- findEndpointZeros (startPoint curve2) curve1 Intersections.FirstCurveIsPoint
  end2Zeros <- findEndpointZeros (endPoint curve2) curve1 Intersections.FirstCurveIsPoint
  Success $
    List.sortAndDeduplicate $
      List.concat $
        [ List.map (\t2 -> Point2d 0.0 t2) start1Zeros
        , List.map (\t2 -> Point2d 1.0 t2) end1Zeros
        , List.map (\t1 -> Point2d t1 0.0) start2Zeros
        , List.map (\t1 -> Point2d t1 1.0) end2Zeros
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
  endpointIntersections <- findEndpointIntersections curve1 curve2
  case overlappingSegments curve1 curve2 endpointIntersections of
    [] -> Result.do
      let derivative1 = derivative curve1
      let derivative2 = derivative curve2
      if VectorCurve2d.hasZero derivative1 || VectorCurve2d.hasZero derivative2
        then Failure Intersections.CurveHasDegeneracy
        else do
          let u = Surface1d.Function.u
          let v = Surface1d.Function.v
          let f = curve1 . u - curve2 . v
          let fu = VectorSurface2d.Function.derivative U f
          let fv = VectorSurface2d.Function.derivative V f
          let g = VectorSurface2d.Function.xy (fu .><. fv) ((derivative1 . u) .<>. f)
          let gu = VectorSurface2d.Function.derivative U g
          let gv = VectorSurface2d.Function.derivative V g
          case Solve2d.search (findIntersectionPoints f fu fv g gu gv endpointIntersections) () of
            Success (NonEmpty points) -> Success (Just (IntersectionPoints points))
            Success [] -> Success Nothing
            Failure Solve2d.InfiniteRecursion -> Failure Intersections.HigherOrderIntersection
    NonEmpty segments -> Success (Just (OverlappingSegments segments))

endpointIntersection :: List UvPoint -> UvBounds -> Maybe UvPoint
endpointIntersection uvPoints uvBounds =
  List.find (\point -> Bounds2d.includes point uvBounds) uvPoints

findIntersectionPoints ::
  Tolerance units =>
  VectorSurface2d.Function.Function (space @ units) ->
  VectorSurface2d.Function.Function (space @ units) ->
  VectorSurface2d.Function.Function (space @ units) ->
  VectorSurface2d.Function.Function (space @ (units :*: units)) ->
  VectorSurface2d.Function.Function (space @ (units :*: units)) ->
  VectorSurface2d.Function.Function (space @ (units :*: units)) ->
  List UvPoint ->
  () ->
  Domain2d ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions () IntersectionPoint
findIntersectionPoints f fu fv g gu gv endpointIntersections () subdomain exclusions = do
  let uvBounds = Domain2d.bounds subdomain
  if not (VectorSurface2d.Function.evaluateBounds f uvBounds ^ Vector2d.zero)
    then Solve2d.pass
    else case exclusions of
      Solve2d.SomeExclusions -> Solve2d.recurse ()
      Solve2d.NoExclusions -> do
        let fuBounds = VectorSurface2d.Function.evaluateBounds fu uvBounds
        let fvBounds = VectorSurface2d.Function.evaluateBounds fv uvBounds
        let domainInterior = Domain2d.interior subdomain
        let validate point constructor sign =
              if Bounds2d.includes point domainInterior
                then do
                  let Point2d t1 t2 = point
                  Solve2d.return (constructor t1 t2 sign)
                else Solve2d.recurse ()
        case Range.resolvedSign (fvBounds .><. fuBounds) of
          Resolved sign -> do
            case endpointIntersection endpointIntersections uvBounds of
              Just point -> validate point IntersectionPoint.crossing sign
              Nothing -> do
                let solution =
                      Solve2d.unique
                        (VectorSurface2d.Function.evaluateBounds f)
                        (VectorSurface2d.Function.evaluate f)
                        (VectorSurface2d.Function.evaluate fu)
                        (VectorSurface2d.Function.evaluate fv)
                        uvBounds
                case solution of
                  Just point -> validate point IntersectionPoint.crossing sign
                  Nothing -> Solve2d.pass
          Unresolved -> do
            let guBounds = VectorSurface2d.Function.evaluateBounds gu uvBounds
            let gvBounds = VectorSurface2d.Function.evaluateBounds gv uvBounds
            case Range.resolvedSign (gvBounds .><. guBounds) of
              Resolved sign -> do
                case endpointIntersection endpointIntersections uvBounds of
                  Just point -> validate point IntersectionPoint.tangent sign
                  Nothing -> do
                    let gBounds = VectorSurface2d.Function.evaluateBounds g uvBounds
                    let convergenceTolerance = 1e-9 * Range.upperBound (VectorBounds2d.magnitude gBounds)
                    let solution =
                          Tolerance.using convergenceTolerance $
                            Solve2d.unique
                              (VectorSurface2d.Function.evaluateBounds g)
                              (VectorSurface2d.Function.evaluate g)
                              (VectorSurface2d.Function.evaluate gu)
                              (VectorSurface2d.Function.evaluate gv)
                              uvBounds
                    case solution of
                      Just point -> validate point IntersectionPoint.tangent sign
                      Nothing -> Solve2d.pass
              Unresolved -> Solve2d.recurse ()

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

transformBy ::
  Transform2d tag (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
transformBy transform curve = case curve of
  Curve c -> Curve (transformByImpl transform c)
  Parametric expression -> Parametric (Expression.Curve2d.transformBy transform expression)
  Coerce c -> Units.coerce (transformBy (Units.coerce transform) c)
  XY{} -> new (TransformBy transform curve)
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
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2d (space @ units1) ->
  Result HasDegeneracy (Curve1d units2)
curvature curve = Result.map Units.specialize (curvature' curve)

data TransformBy curve coordinateSystem where
  TransformBy ::
    Interface curve (space @ units) =>
    Transform2d tag (space @ units) ->
    curve ->
    TransformBy curve (space @ units)

deriving instance Show (TransformBy curve (space @ units))

instance Interface (TransformBy curve (space @ units)) (space @ units) where
  startPointImpl (TransformBy transform curve) =
    Point2d.transformBy transform (startPointImpl curve)

  endPointImpl (TransformBy transform curve) =
    Point2d.transformBy transform (endPointImpl curve)

  evaluateImpl (TransformBy transform curve) tValue =
    Point2d.transformBy transform (evaluateImpl curve tValue)

  evaluateBoundsImpl (TransformBy transform curve) tRange =
    Bounds2d.transformBy transform (evaluateBoundsImpl curve tRange)

  derivativeImpl (TransformBy transform curve) =
    VectorCurve2d.transformBy transform (derivativeImpl curve)

  reverseImpl (TransformBy transform curve) =
    TransformBy transform (reverseImpl curve)

  boundsImpl (TransformBy transform curve) =
    Bounds2d.transformBy transform (boundsImpl curve)

  transformByImpl transform (TransformBy existing curve) =
    new (TransformBy (Transform2d.toAffine existing >> Transform2d.toAffine transform) curve)

removeStartDegeneracy ::
  Int ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
removeStartDegeneracy continuity startCondition curve = Result.do
  let curveDerivatives = Stream.iterate VectorCurve2d.derivative (derivative curve)
  let endDerivativeValues = Stream.map VectorCurve2d.endValue curveDerivatives
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
  show (Synthetic curve _) = Text.unpack ("Synthetic: " + Text.show curve)

-- TODO make Synthetic a first-class constructors,
-- so that it can participate properly in binary operations?
instance Interface (Synthetic (space @ units)) (space @ units) where
  startPointImpl (Synthetic curve _) = startPoint curve

  endPointImpl (Synthetic curve _) = endPoint curve

  evaluateImpl (Synthetic curve _) t = evaluate curve t

  evaluateBoundsImpl (Synthetic curve _) tRange = evaluateBounds curve tRange

  boundsImpl (Synthetic curve _) = bounds curve

  reverseImpl (Synthetic curve curveDerivative) =
    Synthetic (reverse curve) (-(VectorCurve2d.reverse curveDerivative))

  derivativeImpl (Synthetic _ curveDerivative) = curveDerivative

  transformByImpl transform (Synthetic curve curveDerivative) =
    new $
      Synthetic
        (transformBy transform curve)
        (VectorCurve2d.transformBy transform curveDerivative)

data SyntheticDerivative coordinateSystem where
  SyntheticDerivative ::
    VectorCurve2d (space @ units) ->
    ~(VectorCurve2d (space @ units)) ->
    SyntheticDerivative (space @ units)

instance Show (SyntheticDerivative (space @ units)) where
  show (SyntheticDerivative curve _) = Text.unpack ("SyntheticDerivative: " + Text.show curve)

instance VectorCurve2d.Interface (SyntheticDerivative (space @ units)) (space @ units) where
  evaluateImpl (SyntheticDerivative current _) tValue =
    VectorCurve2d.evaluate current tValue

  evaluateBoundsImpl (SyntheticDerivative current _) tRange =
    VectorCurve2d.evaluateBounds current tRange

  derivativeImpl (SyntheticDerivative _ next) = next

  transformByImpl transform (SyntheticDerivative current next) =
    VectorCurve2d.new $
      SyntheticDerivative
        (VectorCurve2d.transformBy transform current)
        (VectorCurve2d.transformBy transform next)

toPolyline ::
  HasCallStack =>
  Qty units ->
  (Float -> vertex) ->
  Curve2d (space @ units) ->
  Polyline2d vertex
toPolyline maxError function curve = do
  let secondDerivative = VectorCurve2d.derivative (derivative curve)
  let epsilon = Qty.abs maxError
  let predicate subdomain = do
        let secondDerivativeBounds = VectorCurve2d.evaluateBounds secondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2d.magnitude secondDerivativeBounds
        let maxSecondDerivativeMagnitude = Range.upperBound secondDerivativeMagnitude
        maxSecondDerivativeMagnitude == Qty.zero
          || Range.width subdomain <= Float.sqrt (8.0 * epsilon / maxSecondDerivativeMagnitude)
  Polyline2d (function 0.0 :| collectVertices predicate function Range.unit [function 1.0])

collectVertices ::
  HasCallStack =>
  (Range Unitless -> Bool) ->
  (Float -> vertex) ->
  Range Unitless ->
  List vertex ->
  List vertex
collectVertices predicate function subdomain accumulated = do
  if predicate subdomain
    then accumulated
    else
      if Range.isAtomic subdomain
        then internalError "Infinite recursion in Curve2d.toPolyline"
        else do
          let (left, right) = Range.bisect subdomain
          let midpoint = Range.midpoint subdomain
          let rightAccumulated = collectVertices predicate function right accumulated
          withFrozenCallStack $
            collectVertices predicate function left (function midpoint : rightAccumulated)

medialAxis ::
  forall space units.
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result MedialAxis.Error (List (MedialAxis.Segment (space @ units)))
medialAxis curve1 curve2 = do
  let p1 = curve1 . Surface1d.Function.u
  let p2 = curve2 . Surface1d.Function.v
  let v1 = derivative curve1 . Surface1d.Function.u
  let v2 = derivative curve2 . Surface1d.Function.v
  let d = p2 - p1
  let target = v2 .><. (2.0 * (v1 .<>. d) .*. d - d .<>. d .*. v1)
  let targetTolerance = ?tolerance .*. ((?tolerance .*. ?tolerance) .*. ?tolerance)
  case Tolerance.using targetTolerance (Surface1d.Function.zeros target) of
    Failure Surface1d.Function.Zeros.HigherOrderZero -> Failure MedialAxis.HigherOrderSolution
    Failure Surface1d.Function.Zeros.ZeroEverywhere -> TODO -- curves are identical arcs?
    Success zeros -> do
      Debug.assert (List.isEmpty (Surface1d.Function.Zeros.crossingLoops zeros))
      Debug.assert (List.isEmpty (Surface1d.Function.Zeros.tangentPoints zeros))
      let allCrossingCurves = List.collect NonEmpty.toList (Surface1d.Function.Zeros.crossingCurves zeros)
      let toSegment (solutionCurve, _) =
            MedialAxis.Segment
              { t1 = Surface1d.Function.u . solutionCurve
              , t2 = Surface1d.Function.v . solutionCurve
              , t12 = solutionCurve
              }
      Success (List.map toSegment allCrossingCurves)

arcLengthParameterization ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve1d Unitless, Qty units)
arcLengthParameterization curve =
  case VectorCurve2d.magnitude (derivative curve) of
    Failure VectorCurve2d.HasZero -> Failure HasDegeneracy
    Success derivativeMagnitude -> Success (ArcLength.parameterization derivativeMagnitude)

parameterizeByArcLength ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve2d (space @ units), Qty units)
parameterizeByArcLength curve = Result.do
  (parameterization, length) <- arcLengthParameterization curve
  Success (curve . parameterization, length)

piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d (space @ units)) ->
  Result HasDegeneracy (Curve2d (space @ units))
piecewise (first :| rest) = Result.do
  parameterizedFirst <- parameterizeByArcLength first
  parameterizedRest <- Result.collect parameterizeByArcLength rest
  let segmentArray = Array.new (parameterizedFirst :| parameterizedRest)
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  Success (new (Piecewise tree arcLength))

data Piecewise (coordinateSystem :: CoordinateSystem) where
  Piecewise :: PiecewiseTree (space @ units) -> Qty units -> Piecewise (space @ units)

instance Show (Piecewise (space @ units)) where
  show _ = Text.unpack "Curve2d.Piecewise"

instance Interface (Piecewise (space @ units)) (space @ units) where
  evaluateImpl (Piecewise tree length) tValue =
    evaluatePiecewise tree (length * tValue)

  evaluateBoundsImpl (Piecewise tree length) (Range tLow tHigh) =
    evaluatePiecewiseBounds tree (length * tLow) (length * tHigh)

  derivativeImpl (Piecewise tree length) =
    VectorCurve2d.new (PiecewiseDerivative (piecewiseTreeDerivative tree length) length)

  transformByImpl piecewiseCurve transform =
    new (TransformBy piecewiseCurve transform)

  reverseImpl (Piecewise tree length) = do
    -- Ignore the computed 'reversed length',
    -- since it should be equal to the original length
    -- but with (if anything) a bit of extra roundoff error
    let (reversedTree, _) = reversePiecewise tree
    Piecewise reversedTree length

buildPiecewiseTree ::
  Array (Curve2d (space @ units), Qty units) ->
  Int ->
  Int ->
  (PiecewiseTree (space @ units), Qty units)
buildPiecewiseTree segmentArray begin end = case end - begin of
  1 -> do
    let (segment, length) = Array.get begin segmentArray
    (PiecewiseLeaf segment length, length)
  n -> do
    Debug.assert (n >= 2)
    let mid = begin + n // 2
    let (leftTree, leftLength) = buildPiecewiseTree segmentArray begin mid
    let (rightTree, rightLength) = buildPiecewiseTree segmentArray mid end
    (PiecewiseNode leftTree leftLength rightTree, leftLength + rightLength)

data PiecewiseTree (coordinateSystem :: CoordinateSystem) where
  PiecewiseNode ::
    PiecewiseTree (space @ units) ->
    Qty units ->
    PiecewiseTree (space @ units) ->
    PiecewiseTree (space @ units)
  PiecewiseLeaf ::
    Curve2d (space @ units) ->
    Qty units ->
    PiecewiseTree (space @ units)

evaluatePiecewise :: PiecewiseTree (space @ units) -> Qty units -> Point2d (space @ units)
evaluatePiecewise tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> evaluatePiecewise leftTree length
    | otherwise -> evaluatePiecewise rightTree (length - leftLength)
  PiecewiseLeaf curve segmentLength -> evaluate curve (length / segmentLength)

evaluatePiecewiseBounds ::
  PiecewiseTree (space @ units) ->
  Qty units ->
  Qty units ->
  Bounds2d (space @ units)
evaluatePiecewiseBounds tree startLength endLength = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        evaluatePiecewiseBounds leftTree startLength endLength
    | startLength >= leftLength ->
        evaluatePiecewiseBounds rightTree (startLength - leftLength) (endLength - leftLength)
    | otherwise ->
        Bounds2d.aggregate2
          (evaluatePiecewiseBounds leftTree startLength leftLength)
          (evaluatePiecewiseBounds rightTree Qty.zero (endLength - leftLength))
  PiecewiseLeaf curve segmentLength ->
    evaluateBounds curve (Range.from (startLength / segmentLength) (endLength / segmentLength))

reversePiecewise :: PiecewiseTree (space @ units) -> (PiecewiseTree (space @ units), Qty units)
reversePiecewise tree = case tree of
  PiecewiseNode leftTree _ rightTree -> do
    let (reversedRight, rightLength) = reversePiecewise rightTree
    let (reversedLeft, leftLength) = reversePiecewise leftTree
    (PiecewiseNode reversedRight rightLength reversedLeft, rightLength + leftLength)
  PiecewiseLeaf curve length -> (PiecewiseLeaf (reverse curve) length, length)

data PiecewiseDerivative (coordinateSystem :: CoordinateSystem) where
  PiecewiseDerivative ::
    PiecewiseDerivativeTree (space @ units) ->
    Qty units ->
    PiecewiseDerivative (space @ units)

instance Show (PiecewiseDerivative (space @ units)) where
  show _ = Text.unpack "Curve2d.PiecewiseDerivative"

instance VectorCurve2d.Interface (PiecewiseDerivative (space @ units)) (space @ units) where
  evaluateImpl (PiecewiseDerivative tree length) tValue =
    evaluatePiecewiseDerivative tree (length * tValue)

  evaluateBoundsImpl (PiecewiseDerivative tree length) (Range tLow tHigh) =
    evaluatePiecewiseDerivativeBounds tree (length * tLow) (length * tHigh)

  derivativeImpl (PiecewiseDerivative tree length) =
    VectorCurve2d.new (PiecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

  transformByImpl transform piecewiseCurve =
    VectorCurve2d.Transformed
      (Units.erase (Transform2d.toAffine transform))
      (VectorCurve2d.new piecewiseCurve)

data PiecewiseDerivativeTree (coordinateSystem :: CoordinateSystem) where
  PiecewiseDerivativeNode ::
    PiecewiseDerivativeTree (space @ units) ->
    Qty units ->
    PiecewiseDerivativeTree (space @ units) ->
    PiecewiseDerivativeTree (space @ units)
  PiecewiseDerivativeLeaf ::
    VectorCurve2d (space @ units) ->
    Qty units ->
    PiecewiseDerivativeTree (space @ units)

piecewiseTreeDerivative ::
  PiecewiseTree (space @ units) ->
  Qty units ->
  PiecewiseDerivativeTree (space @ units)
piecewiseTreeDerivative tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseTreeDerivative leftTree length)
      leftLength
      (piecewiseTreeDerivative rightTree length)
  PiecewiseLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf ((length / segmentLength) * derivative curve) segmentLength

piecewiseDerivativeTreeDerivative ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  PiecewiseDerivativeTree (space @ units)
piecewiseDerivativeTreeDerivative tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseDerivativeTreeDerivative leftTree length)
      leftLength
      (piecewiseDerivativeTreeDerivative rightTree length)
  PiecewiseDerivativeLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf
      ((length / segmentLength) * VectorCurve2d.derivative curve)
      segmentLength

evaluatePiecewiseDerivative ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  Vector2d (space @ units)
evaluatePiecewiseDerivative tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> evaluatePiecewiseDerivative leftTree length
    | otherwise -> evaluatePiecewiseDerivative rightTree (length - leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluate curve (length / segmentLength)

evaluatePiecewiseDerivativeBounds ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  Qty units ->
  VectorBounds2d (space @ units)
evaluatePiecewiseDerivativeBounds tree startLength endLength = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        evaluatePiecewiseDerivativeBounds leftTree startLength endLength
    | startLength >= leftLength ->
        evaluatePiecewiseDerivativeBounds
          rightTree
          (startLength - leftLength)
          (endLength - leftLength)
    | otherwise ->
        VectorBounds2d.aggregate2
          (evaluatePiecewiseDerivativeBounds leftTree startLength leftLength)
          (evaluatePiecewiseDerivativeBounds rightTree Qty.zero (endLength - leftLength))
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluateBounds curve $
      Range.from (startLength / segmentLength) (endLength / segmentLength)
