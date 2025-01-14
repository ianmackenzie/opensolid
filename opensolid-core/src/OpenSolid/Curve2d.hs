module OpenSolid.Curve2d
  ( Curve2d (Parametric, Transformed)
  , pattern Point
  , HasDegeneracy (HasDegeneracy)
  , Interface (..)
  , new
  , constant
  , xy
  , line
  , arc
  , polarArc
  , sweptArc
  , cornerArc
  , WhichArc (..)
  , radiusArc
  , ellipticalArc
  , customArc
  , circle
  , ellipse
  , bezier
  , quadraticBezier
  , cubicBezier
  , hermite
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
  , samplingPoints
  , medialAxis
  , arcLengthParameterization
  , unsafeArcLengthParameterization
  , parameterizeByArcLength
  , unsafeParameterizeByArcLength
  , piecewise
  , unsafePiecewise
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Arithmetic qualified as Arithmetic
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d.FindPoint qualified as FindPoint
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve2d.MedialAxis qualified as MedialAxis
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2d.OverlappingSegment qualified as OverlappingSegment
import OpenSolid.Debug qualified as Debug
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Domain1d qualified as Domain1d
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
import OpenSolid.Linearization qualified as Linearization
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
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
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
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
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
    Curve units ->
    Curve units ->
    Curve2d (space @ units)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Curve2d (local @ units) ->
    Curve2d (global @ units)
  Transformed ::
    Transform2d tag (space @ units) ->
    Curve2d (space @ units) ->
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
  reverseImpl :: curve -> Curve2d coordinateSystem
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
  lhs + rhs = new (Arithmetic.Sum lhs rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Interface
    (Arithmetic.Sum (Curve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2)))
    (space1 @ units1)
  where
  evaluateImpl (Arithmetic.Sum curve vectorCurve) tValue =
    evaluate curve tValue + VectorCurve2d.evaluate vectorCurve tValue

  evaluateBoundsImpl (Arithmetic.Sum curve vectorCurve) tRange =
    evaluateBounds curve tRange + VectorCurve2d.evaluateBounds vectorCurve tRange

  derivativeImpl (Arithmetic.Sum curve vectorCurve) =
    derivative curve + VectorCurve2d.derivative vectorCurve

  reverseImpl (Arithmetic.Sum curve vectorCurve) =
    reverse curve + VectorCurve2d.reverse vectorCurve

  transformByImpl transform (Arithmetic.Sum curve vectorCurve) =
    transformBy transform curve + VectorCurve2d.transformBy transform vectorCurve

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
  lhs - rhs = new (Arithmetic.Difference lhs rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Interface
    (Arithmetic.Difference (Curve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2)))
    (space1 @ units1)
  where
  evaluateImpl (Arithmetic.Difference curve vectorCurve) tValue =
    evaluate curve tValue - VectorCurve2d.evaluate vectorCurve tValue

  evaluateBoundsImpl (Arithmetic.Difference curve vectorCurve) tRange =
    evaluateBounds curve tRange - VectorCurve2d.evaluateBounds vectorCurve tRange

  derivativeImpl (Arithmetic.Difference curve vectorCurve) =
    derivative curve - VectorCurve2d.derivative vectorCurve

  reverseImpl (Arithmetic.Difference curve vectorCurve) =
    reverse curve - VectorCurve2d.reverse vectorCurve

  transformByImpl transform (Arithmetic.Difference curve vectorCurve) =
    transformBy transform curve - VectorCurve2d.transformBy transform vectorCurve

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

instance Composition (Curve Unitless) (Curve2d (space @ units)) (Curve2d (space @ units)) where
  Parametric outer . Curve.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance Interface (Curve2d (space @ units) :.: Curve Unitless) (space @ units) where
  evaluateImpl (curve2d :.: curve1d) tRange =
    evaluate curve2d (Curve.evaluate curve1d tRange)

  evaluateBoundsImpl (curve2d :.: curve1d) tRange =
    evaluateBounds curve2d (Curve.evaluateBounds curve1d tRange)

  derivativeImpl (curve2d :.: curve1d) =
    (derivative curve2d . curve1d) * Curve.derivative curve1d

  reverseImpl (curve2d :.: curve1d) =
    new (curve2d :.: Curve.reverse curve1d)

  transformByImpl transform (curve2d :.: curve1d) =
    new (transformBy transform curve2d . curve1d)

instance
  Composition
    (SurfaceFunction Unitless)
    (Curve2d (space @ units))
    (SurfaceFunction2d (space @ units))
  where
  Parametric outer . SurfaceFunction.Parametric inner = SurfaceFunction2d.Parametric (outer . inner)
  curve . function = SurfaceFunction2d.new (curve :.: function)

instance
  SurfaceFunction2d.Interface
    (Curve2d (space @ units) :.: SurfaceFunction Unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: function) uvPoint =
    evaluate curve (SurfaceFunction.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    evaluateBounds curve (SurfaceFunction.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (derivative curve . function) * SurfaceFunction.derivative parameter function

  transformByImpl transform (curve :.: function) =
    transformBy transform curve . function

instance
  Composition
    (Curve2d UvCoordinates)
    (SurfaceFunction units)
    (Curve units)
  where
  SurfaceFunction.Parametric outer . Parametric inner = Curve.Parametric (outer . inner)
  outer . inner = Curve.new (outer :.: inner)

instance Curve.Interface (SurfaceFunction units :.: Curve2d UvCoordinates) units where
  evaluateImpl (function :.: uvCurve) t =
    SurfaceFunction.evaluate function (evaluate uvCurve t)

  evaluateBoundsImpl (function :.: uvCurve) t =
    SurfaceFunction.evaluateBounds function (evaluateBounds uvCurve t)

  derivativeImpl (function :.: uvCurve) = do
    let fU = SurfaceFunction.derivative U function
    let fV = SurfaceFunction.derivative V function
    let uvT = derivative uvCurve
    let uT = VectorCurve2d.xComponent uvT
    let vT = VectorCurve2d.yComponent uvT
    fU . uvCurve * uT + fV . uvCurve * vT

new :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
new = Curve

constant :: Point2d (space @ units) -> Curve2d (space @ units)
constant = Parametric . Expression.constant

xy :: Curve units -> Curve units -> Curve2d (space @ units)
xy (Curve.Parametric x) (Curve.Parametric y) = Parametric (Expression.xy x y)
xy x y = XY x y

line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
line p1 p2 = p1 + Curve.t * (p2 - p1)

arc ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Angle ->
  Curve2d (space @ units)
arc givenStartPoint givenEndPoint sweptAngle =
  case Vector2d.magnitudeAndDirection (givenEndPoint - givenStartPoint) of
    Failure Vector2d.IsZero -> line givenStartPoint givenEndPoint
    Success (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 * distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 * sweptAngle)
      let linearDeviation = halfDistance * tanHalfAngle
      if linearDeviation ~= Qty.zero
        then line givenStartPoint givenEndPoint
        else do
          let offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeft directionBetweenPoints
          let centerPoint = Point2d.midpoint givenStartPoint givenEndPoint + offset
          let radius = Point2d.distanceFrom centerPoint givenStartPoint
          let xVector = Vector2d.x radius
          let yVector = Vector2d.y radius
          let startAngle = Point2d.angleFrom centerPoint givenStartPoint
          let endAngle = startAngle + sweptAngle
          customArc centerPoint xVector yVector startAngle endAngle

polarArc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
polarArc centerPoint radius startAngle endAngle =
  customArc centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle

sweptArc :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Curve2d (space @ units)
sweptArc centerPoint givenStartPoint sweptAngle = do
  let radius = Point2d.distanceFrom centerPoint givenStartPoint
  let startAngle = Point2d.angleFrom centerPoint givenStartPoint
  polarArc centerPoint radius startAngle (startAngle + sweptAngle)

cornerArc ::
  Tolerance units =>
  Point2d (space @ units) ->
  Direction2d space ->
  Direction2d space ->
  Qty units ->
  Curve2d (space @ units)
cornerArc cornerPoint incomingDirection outgoingDirection givenRadius = do
  let radius = Qty.abs givenRadius
  let sweptAngle = Direction2d.angleFrom incomingDirection outgoingDirection
  if radius * Float.squared (Angle.inRadians sweptAngle) / 4.0 ~= Qty.zero
    then line cornerPoint cornerPoint
    else do
      let offset = radius * Qty.abs (Angle.tan (0.5 * sweptAngle))
      let computedStartPoint = cornerPoint - offset * incomingDirection
      let computedEndPoint = cornerPoint + offset * outgoingDirection
      arc computedStartPoint computedEndPoint sweptAngle

data WhichArc
  = SmallCounterclockwise
  | SmallClockwise
  | LargeCounterclockwise
  | LargeClockwise

radiusArc ::
  Tolerance units =>
  Qty units ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  WhichArc ->
  Curve2d (space @ units)
radiusArc givenRadius givenStartPoint givenEndPoint whichArc =
  case Direction2d.from givenStartPoint givenEndPoint of
    Success chordDirection -> do
      let halfDistance = 0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint
      let radius = Qty.max (Qty.abs givenRadius) halfDistance
      let offsetMagnitude = Qty.sqrt' (Qty.squared' radius - Qty.squared' halfDistance)
      let offsetDirection = Direction2d.rotateLeft chordDirection
      let offsetDistance =
            case whichArc of
              SmallCounterclockwise -> offsetMagnitude
              SmallClockwise -> -offsetMagnitude
              LargeClockwise -> offsetMagnitude
              LargeCounterclockwise -> -offsetMagnitude
      let offset = offsetDirection * offsetDistance
      let centerPoint = Point2d.midpoint givenStartPoint givenEndPoint + offset
      let shortAngle = 2.0 * Angle.asin (halfDistance / givenRadius)
      let sweptAngle =
            case whichArc of
              SmallCounterclockwise -> shortAngle
              SmallClockwise -> -shortAngle
              LargeClockwise -> shortAngle - Angle.fullTurn
              LargeCounterclockwise -> Angle.fullTurn - shortAngle
      sweptArc centerPoint givenStartPoint sweptAngle
    Failure Direction2d.PointsAreCoincident ->
      line givenStartPoint givenEndPoint

ellipticalArc ::
  Frame2d (space @ units) defines ->
  Qty units ->
  Qty units ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
ellipticalArc axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2d.originPoint axes
  let xVector = xRadius * Frame2d.xDirection axes
  let yVector = yRadius * Frame2d.yDirection axes
  customArc centerPoint xVector yVector startAngle endAngle

customArc ::
  Point2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
customArc p0 v1 v2 a b = do
  let angle = Curve.line a b
  p0 + v1 * Curve.cos angle + v2 * Curve.sin angle

circle :: Point2d (space @ units) -> Qty units -> Curve2d (space @ units)
circle centerPoint radius = polarArc centerPoint radius Angle.zero Angle.twoPi

ellipse :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Curve2d (space @ units)
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points. For example,

> Curve2d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2d (space @ units)) -> Curve2d (space @ units)
bezier controlPoints = do
  let x = Curve.bezier (NonEmpty.map Point2d.xCoordinate controlPoints)
  let y = Curve.bezier (NonEmpty.map Point2d.yCoordinate controlPoints)
  XY x y

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
quadraticBezier p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
cubicBezier p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

{-| Construct a Bezier curve with the given start point, start derivatives, end point and end
derivatives. For example,

> Curve2d.hermite (p1, [v1]) (p2, [v2])

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve2d.hermite (p1, [v1]) (p2, [])

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units)
hermite (Point2d xStart yStart, startDerivatives) (Point2d xEnd yEnd, endDerivatives) = do
  let xStartDerivatives = List.map Vector2d.xComponent startDerivatives
  let yStartDerivatives = List.map Vector2d.yComponent startDerivatives
  let xEndDerivatives = List.map Vector2d.xComponent endDerivatives
  let yEndDerivatives = List.map Vector2d.yComponent endDerivatives
  let x = Curve.hermite (xStart, xStartDerivatives) (xEnd, xEndDerivatives)
  let y = Curve.hermite (yStart, yStartDerivatives) (yEnd, yEndDerivatives)
  XY x y

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint curve = case curve of
  Curve c -> startPointImpl c
  Parametric expresssion -> Expression.evaluate expresssion 0.0
  Coerce c -> Units.coerce (startPoint c)
  XY x y -> Point2d.xy (Curve.evaluate x 0.0) (Curve.evaluate y 0.0)
  PlaceIn frame c -> Point2d.placeIn frame (startPoint c)
  Transformed transform c -> Point2d.transformBy transform (startPoint c)

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint curve = case curve of
  Curve c -> endPointImpl c
  Parametric expresssion -> Expression.evaluate expresssion 1.0
  Coerce c -> Units.coerce (endPoint c)
  XY x y -> Point2d.xy (Curve.evaluate x 1.0) (Curve.evaluate y 1.0)
  PlaceIn frame c -> Point2d.placeIn frame (endPoint c)
  Transformed transform c -> Point2d.transformBy transform (endPoint c)

evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluate curve tValue = case curve of
  Curve c -> evaluateImpl c tValue
  Parametric expresssion -> Expression.evaluate expresssion tValue
  Coerce c -> Units.coerce (evaluate c tValue)
  XY x y -> Point2d.xy (Curve.evaluate x tValue) (Curve.evaluate y tValue)
  PlaceIn frame c -> Point2d.placeIn frame (evaluate c tValue)
  Transformed transform c -> Point2d.transformBy transform (evaluate c tValue)

evaluateBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
evaluateBounds curve tRange = case curve of
  Curve c -> evaluateBoundsImpl c tRange
  Parametric expresssion -> Expression.evaluateBounds expresssion tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  XY x y -> Bounds2d.xy (Curve.evaluateBounds x tRange) (Curve.evaluateBounds y tRange)
  PlaceIn frame c -> Bounds2d.placeIn frame (evaluateBounds c tRange)
  Transformed transform c -> Bounds2d.transformBy transform (evaluateBounds c tRange)

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve = case curve of
  Curve c -> derivativeImpl c
  Parametric expresssion -> VectorCurve2d.Parametric (Expression.curveDerivative expresssion)
  XY x y -> VectorCurve2d.xy (Curve.derivative x) (Curve.derivative y)
  Coerce c -> Units.coerce (derivative c)
  PlaceIn frame c -> VectorCurve2d.placeIn frame (derivative c)
  Transformed transform c -> VectorCurve2d.transformBy transform (derivative c)

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse curve = case curve of
  Curve c -> Curve (reverseImpl c)
  Parametric expression -> Parametric (expression . Expression.r)
  XY x y -> XY (Curve.reverse x) (Curve.reverse y)
  Coerce c -> Units.coerce (reverse c)
  PlaceIn frame c -> PlaceIn frame (reverse c)
  Transformed transform c -> Transformed transform (reverse c)

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds curve = case curve of
  Curve c -> boundsImpl c
  Parametric expression -> Expression.evaluateBounds expression Range.unit
  Coerce c -> Units.coerce (bounds c)
  XY x y -> Bounds2d.xy (Curve.evaluateBounds x Range.unit) (Curve.evaluateBounds y Range.unit)
  PlaceIn frame c -> Bounds2d.placeIn frame (bounds c)
  Transformed transform c -> Bounds2d.transformBy transform (bounds c)

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

signedDistanceAlong :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve units
signedDistanceAlong axis curve = (curve - Axis2d.originPoint axis) <> Axis2d.direction axis

xCoordinate :: Curve2d (space @ units) -> Curve units
xCoordinate = signedDistanceAlong Axis2d.x

yCoordinate :: Curve2d (space @ units) -> Curve units
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
          let u = SurfaceFunction.u
          let v = SurfaceFunction.v
          let f = curve1 . u - curve2 . v
          let fu = VectorSurfaceFunction2d.derivative U f
          let fv = VectorSurfaceFunction2d.derivative V f
          let g = VectorSurfaceFunction2d.xy (fu .><. fv) ((derivative1 . u) .<>. f)
          let gu = VectorSurfaceFunction2d.derivative U g
          let gv = VectorSurfaceFunction2d.derivative V g
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
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ (units :*: units)) ->
  VectorSurfaceFunction2d (space @ (units :*: units)) ->
  VectorSurfaceFunction2d (space @ (units :*: units)) ->
  List UvPoint ->
  () ->
  Domain2d ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions () IntersectionPoint
findIntersectionPoints f fu fv g gu gv endpointIntersections () subdomain exclusions = do
  let uvBounds = Domain2d.bounds subdomain
  if not (VectorSurfaceFunction2d.evaluateBounds f uvBounds ^ Vector2d.zero)
    then Solve2d.pass
    else case exclusions of
      Solve2d.SomeExclusions -> Solve2d.recurse ()
      Solve2d.NoExclusions -> do
        let fuBounds = VectorSurfaceFunction2d.evaluateBounds fu uvBounds
        let fvBounds = VectorSurfaceFunction2d.evaluateBounds fv uvBounds
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
                        (VectorSurfaceFunction2d.evaluateBounds f)
                        (VectorSurfaceFunction2d.evaluate f)
                        (VectorSurfaceFunction2d.evaluate fu)
                        (VectorSurfaceFunction2d.evaluate fv)
                        uvBounds
                case solution of
                  Just point -> validate point IntersectionPoint.crossing sign
                  Nothing -> Solve2d.pass
          Unresolved -> do
            let guBounds = VectorSurfaceFunction2d.evaluateBounds gu uvBounds
            let gvBounds = VectorSurfaceFunction2d.evaluateBounds gv uvBounds
            case Range.resolvedSign (gvBounds .><. guBounds) of
              Resolved sign -> do
                case endpointIntersection endpointIntersections uvBounds of
                  Just point -> validate point IntersectionPoint.tangent sign
                  Nothing -> do
                    let gBounds = VectorSurfaceFunction2d.evaluateBounds g uvBounds
                    let convergenceTolerance = 1e-9 * Range.upperBound (VectorBounds2d.magnitude gBounds)
                    let solution =
                          Tolerance.using convergenceTolerance $
                            Solve2d.unique
                              (VectorSurfaceFunction2d.evaluateBounds g)
                              (VectorSurfaceFunction2d.evaluate g)
                              (VectorSurfaceFunction2d.evaluate gu)
                              (VectorSurfaceFunction2d.evaluate gv)
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
  XY{} -> Transformed transform curve
  PlaceIn frame c -> PlaceIn frame (transformBy (Transform2d.relativeTo frame transform) c)
  Transformed existing c ->
    Transformed (Transform2d.toAffine transform . Transform2d.toAffine existing) c

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
  Result HasDegeneracy (Curve (Unitless :/: units))
curvature' curve = Result.do
  let firstDerivative = derivative curve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  tangent <- tangentDirection curve
  Success ((tangent >< secondDerivative) !/!. (firstDerivative .<>. firstDerivative))

curvature ::
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2d (space @ units1) ->
  Result HasDegeneracy (Curve units2)
curvature curve = Result.map Units.specialize (curvature' curve)

removeStartDegeneracy ::
  Int ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
removeStartDegeneracy continuity startCondition curve = Result.do
  let curveDerivatives = Stream.iterate VectorCurve2d.derivative (derivative curve)
  let endDerivativeValues = Stream.map VectorCurve2d.endValue curveDerivatives
  let endCondition endDegree = (endPoint curve, Stream.take endDegree endDerivativeValues)
  let baseCurve endDegree = hermite startCondition (endCondition endDegree)
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
    new (Synthetic (reverse curve) (-(VectorCurve2d.reverse curveDerivative)))

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

toPolyline :: Qty units -> Curve2d (space @ units) -> Polyline2d (Point2d (space @ units))
toPolyline maxError curve =
  Polyline2d (NonEmpty.map (evaluate curve) (samplingPoints maxError curve))

samplingPoints :: Qty units -> Curve2d (space @ units) -> NonEmpty Float
samplingPoints maxError curve = do
  let secondDerivative = VectorCurve2d.derivative (derivative curve)
  let predicate subdomain = do
        let secondDerivativeBounds = VectorCurve2d.evaluateBounds secondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2d.magnitude secondDerivativeBounds
        Linearization.error subdomain secondDerivativeMagnitude <= Qty.abs maxError
  Domain1d.samplingPoints predicate

medialAxis ::
  forall space units.
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result MedialAxis.Error (List (MedialAxis.Segment (space @ units)))
medialAxis curve1 curve2 = do
  let p1 = curve1 . SurfaceFunction.u
  let p2 = curve2 . SurfaceFunction.v
  let v1 = derivative curve1 . SurfaceFunction.u
  let v2 = derivative curve2 . SurfaceFunction.v
  let d = p2 - p1
  let target = v2 .><. (2.0 * (v1 .<>. d) .*. d - d .<>. d .*. v1)
  let targetTolerance = ?tolerance .*. ((?tolerance .*. ?tolerance) .*. ?tolerance)
  case Tolerance.using targetTolerance (SurfaceFunction.zeros target) of
    Failure SurfaceFunction.Zeros.HigherOrderZero -> Failure MedialAxis.HigherOrderSolution
    Failure SurfaceFunction.Zeros.ZeroEverywhere -> TODO -- curves are identical arcs?
    Success zeros -> do
      Debug.assert (List.isEmpty (SurfaceFunction.Zeros.crossingLoops zeros))
      Debug.assert (List.isEmpty (SurfaceFunction.Zeros.tangentPoints zeros))
      case Result.map DirectionCurve2d.unwrap (tangentDirection curve1) of
        Success tangent1 -> do
          let normal1 = VectorCurve2d.rotateBy Angle.quarterTurn tangent1
          let radius :: SurfaceFunction units =
                (d .<>. d) .!/! (2.0 * (tangent1 . SurfaceFunction.u) >< d)
          let curve :: SurfaceFunction2d (space @ units) =
                (curve1 . SurfaceFunction.u) + radius * (normal1 . SurfaceFunction.u)
          let toSegment solutionCurve =
                MedialAxis.Segment
                  { t1 = SurfaceFunction.u . solutionCurve
                  , t2 = SurfaceFunction.v . solutionCurve
                  , t12 = solutionCurve
                  , curve = curve . solutionCurve
                  , radius = radius . solutionCurve
                  }
          Success (List.map toSegment (SurfaceFunction.Zeros.crossingCurves zeros))
        Failure HasDegeneracy -> Failure MedialAxis.DegenerateCurve

arcLengthParameterization ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve Unitless, Qty units)
arcLengthParameterization curve =
  case VectorCurve2d.magnitude (derivative curve) of
    Failure VectorCurve2d.HasZero -> Failure HasDegeneracy
    Success derivativeMagnitude -> Success (ArcLength.parameterization derivativeMagnitude)

unsafeArcLengthParameterization :: Curve2d (space @ units) -> (Curve Unitless, Qty units)
unsafeArcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve2d.unsafeMagnitude (derivative curve))

parameterizeByArcLength ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve2d (space @ units), Qty units)
parameterizeByArcLength curve = Result.do
  (parameterization, length) <- arcLengthParameterization curve
  Success (curve . parameterization, length)

unsafeParameterizeByArcLength :: Curve2d (space @ units) -> (Curve2d (space @ units), Qty units)
unsafeParameterizeByArcLength curve = do
  let (parameterization, length) = unsafeArcLengthParameterization curve
  (curve . parameterization, length)

piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d (space @ units)) ->
  Result HasDegeneracy (Curve2d (space @ units))
piecewise (first :| rest) = Result.do
  parameterizedFirst <- parameterizeByArcLength first
  parameterizedRest <- Result.collect parameterizeByArcLength rest
  let segmentArray = Array.fromNonEmpty (parameterizedFirst :| parameterizedRest)
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  Success (new (Piecewise tree arcLength))

unsafePiecewise :: NonEmpty (Curve2d (space @ units)) -> Curve2d (space @ units)
unsafePiecewise segments = do
  let segmentArray = Array.fromNonEmpty (NonEmpty.map unsafeParameterizeByArcLength segments)
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  new (Piecewise tree arcLength)

data Piecewise (coordinateSystem :: CoordinateSystem) where
  Piecewise :: PiecewiseTree (space @ units) -> Qty units -> Piecewise (space @ units)

deriving instance Show (Piecewise (space @ units))

instance Interface (Piecewise (space @ units)) (space @ units) where
  evaluateImpl (Piecewise tree length) tValue =
    evaluatePiecewise tree (length * tValue)

  evaluateBoundsImpl (Piecewise tree length) (Range tLow tHigh) =
    evaluatePiecewiseBounds tree (length * tLow) (length * tHigh)

  derivativeImpl (Piecewise tree length) =
    VectorCurve2d.new (PiecewiseDerivative (piecewiseTreeDerivative tree length) length)

  transformByImpl transform piecewiseCurve =
    Transformed transform (new piecewiseCurve)

  reverseImpl (Piecewise tree length) = do
    -- Ignore the computed 'reversed length',
    -- since it should be equal to the original length
    -- but with (if anything) a bit of extra roundoff error
    let (reversedTree, _) = reversePiecewise tree
    new (Piecewise reversedTree length)

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

deriving instance Show (PiecewiseTree (space @ units))

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

deriving instance Show (PiecewiseDerivative (space @ units))

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

deriving instance Show (PiecewiseDerivativeTree (space @ units))

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
