module OpenSolid.Curve2d
  ( Curve2d
  , pattern Point
  , HasDegeneracy (HasDegeneracy)
  , Compiled
  , new
  , recursive
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
  , synthetic
  , evaluate
  , evaluateBounds
  , tangentDirection
  , offsetLeftwardBy
  , offsetRightwardBy
  , reverse
  , bounds
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , OverlappingSegment
  , intersections
  , IsCoincidentWithPoint (IsCoincidentWithPoint)
  , findPoint
  , distanceAlong
  , distanceLeftOf
  , distanceRightOf
  , isOnAxis
  , xCoordinate
  , yCoordinate
  , placeIn
  , relativeTo
  , on
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , convert
  , unconvert
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
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2d (Axis2d (Axis2d))
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve2d.MedialAxis qualified as MedialAxis
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment))
import OpenSolid.Curve2d.OverlappingSegment qualified as OverlappingSegment
import {-# SOURCE #-} OpenSolid.Curve3d (Curve3d)
import {-# SOURCE #-} OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Debug qualified as Debug
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Domain2d (Domain2d)
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polyline2d (Polyline2d (Polyline2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.Stream qualified as Stream
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Zeros qualified as SurfaceFunction.Zeros
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

-- | A parametric curve in 2D space.
data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d :: Compiled (space @ units) -> ~(VectorCurve2d (space @ units)) -> Curve2d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Point2d coordinateSystem)
    (Bounds Unitless)
    (Bounds2d coordinateSystem)

instance HasField "compiled" (Curve2d (space @ units)) (Compiled (space @ units)) where
  getField (Curve2d c _) = c

instance HasField "derivative" (Curve2d (space @ units)) (VectorCurve2d (space @ units)) where
  getField (Curve2d _ d) = d

instance FFI (Curve2d (space @ Meters)) where
  representation = FFI.classRepresentation "Curve2d"

instance FFI (Curve2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvCurve"

instance HasUnits (Curve2d (space @ units)) units (Curve2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Curve2d (space1 @ unitsA)) (Curve2d (space2 @ unitsB))
  where
  coerce curve = Curve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

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
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Curve2d (space2 @ units2)) units1
  where
  curve1 ~= curve2 = samplePoints curve1 ~= samplePoints curve2

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Curve2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  curve ~= point = List.allSatisfy (~= point) (samplePoints curve)

pattern Point :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units)
pattern Point point <- (asPoint -> Just point)

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Error.Message)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  lhs + rhs =
    new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  lhs - rhs =
    new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (Curve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  lhs - rhs = VectorCurve2d.new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

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

instance
  unitless ~ Unitless =>
  Composition (Curve unitless) (Curve2d (space @ units)) (Curve2d (space @ units))
  where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (Curve2d (space @ units))
    (SurfaceFunction2d (space @ units))
  where
  curve . function =
    SurfaceFunction2d.new
      # curve.compiled . function.compiled
      # \p -> curve.derivative . function * SurfaceFunction.derivative p function

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction units)
    (Curve units)
  where
  f . g = do
    let dfdu = SurfaceFunction.derivative U f
    let dfdv = SurfaceFunction.derivative V f
    let dgdt = g.derivative
    let dudt = dgdt.xComponent
    let dvdt = dgdt.yComponent
    Curve.new (f.compiled . g.compiled) (dfdu . g * dudt + dfdv . g * dvdt)

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (VectorSurfaceFunction3d (space @ units))
    (VectorCurve3d (space @ units))
  where
  function . uvCurve = do
    let fU = VectorSurfaceFunction3d.derivative U function
    let fV = VectorSurfaceFunction3d.derivative V function
    let uvT = uvCurve.derivative
    let uT = uvT.xComponent
    let vT = uvT.yComponent
    VectorCurve3d.new
      # function.compiled . uvCurve.compiled
      # fU . uvCurve * uT + fV . uvCurve * vT

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction3d (space @ units))
    (Curve3d (space @ units))
  where
  function . uvCurve = do
    let fU = SurfaceFunction3d.derivative U function
    let fV = SurfaceFunction3d.derivative V function
    let uvT = uvCurve.derivative
    let uT = uvT.xComponent
    let vT = uvT.yComponent
    Curve3d.new
      # function.compiled . uvCurve.compiled
      # fU . uvCurve * uT + fV . uvCurve * vT

new :: Compiled (space @ units) -> VectorCurve2d (space @ units) -> Curve2d (space @ units)
new = Curve2d

recursive ::
  Compiled (space @ units) ->
  (Curve2d (space @ units) -> VectorCurve2d (space @ units)) ->
  Curve2d (space @ units)
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | Create a degenerate curve that is actually just a single point.
constant :: Point2d (space @ units) -> Curve2d (space @ units)
constant point = new (CompiledFunction.constant point) VectorCurve2d.zero

-- | Create a curve from its X and Y coordinate curves.
xy :: Curve units -> Curve units -> Curve2d (space @ units)
xy x y =
  new
    # CompiledFunction.map2
      Expression.xy
      Point2d
      Bounds2d
      x.compiled
      y.compiled
    # VectorCurve2d.xy x.derivative y.derivative

-- | Create a line between two points.
line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
line p1 p2 = bezier (NonEmpty.two p1 p2)

{-| Create an arc with the given start point, end point and swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
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
          let offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeftward directionBetweenPoints
          let centerPoint = Point2d.midpoint givenStartPoint givenEndPoint + offset
          let radius = Point2d.distanceFrom centerPoint givenStartPoint
          let xVector = Vector2d.x radius
          let yVector = Vector2d.y radius
          let startAngle = Point2d.angleFrom centerPoint givenStartPoint
          let endAngle = startAngle + sweptAngle
          customArc centerPoint xVector yVector startAngle endAngle

-- | Create an arc with the given center point, radius, start angle and end angle.
polarArc ::
  ( "centerPoint" ::: Point2d (space @ units)
  , "radius" ::: Qty units
  , "startAngle" ::: Angle
  , "endAngle" ::: Angle
  ) ->
  Curve2d (space @ units)
polarArc (Field centerPoint, Field radius, Field startAngle, Field endAngle) =
  customArc centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle

{-| Create an arc with the given center point, start point and swept angle.

The start point will be swept around the center point by the given angle.
-}
sweptArc :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Curve2d (space @ units)
sweptArc centerPoint givenStartPoint sweptAngle = do
  let radius = Point2d.distanceFrom centerPoint givenStartPoint
  let startAngle = Point2d.angleFrom centerPoint givenStartPoint
  polarArc do
    #centerPoint centerPoint
    #radius radius
    #startAngle startAngle
    #endAngle (startAngle + sweptAngle)

-- | Create an arc for rounding off the corner between two straight lines.
cornerArc ::
  Tolerance units =>
  Point2d (space @ units) ->
  Direction2d space ->
  Direction2d space ->
  "radius" ::: Qty units ->
  Curve2d (space @ units)
cornerArc cornerPoint incomingDirection outgoingDirection (Field givenRadius) = do
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
      let offsetDirection = Direction2d.rotateLeftward chordDirection
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

-- | Create a circle with the given center point and diameter.
circle ::
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Curve2d (space @ units)
circle (Field centerPoint, Field diameter) =
  polarArc do
    #centerPoint centerPoint
    #radius (0.5 * diameter)
    #startAngle Angle.zero
    #endAngle Angle.twoPi

{-| Create an ellipes with the given principal axes and major/minor radii.
The first radius given will be the radius along the X axis,
and the second radius will be the radius along the Y axis.
-}
ellipse :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Curve2d (space @ units)
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points.

For example,

> Curve2d.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2d (space @ units)) -> Curve2d (space @ units)
bezier controlPoints =
  new
    # CompiledFunction.concrete (Expression.bezierCurve controlPoints)
    # VectorCurve2d.bezierCurve (Bezier.derivative controlPoints)

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

{-| Construct a Bezier curve with the given endpoints and derivatives at those endpoints.

For example,

> Curve2d.hermite p1 [v1] p2 [v2]

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve2d.hermite p1 [v1] p2 []

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Curve2d (space @ units)
hermite start startDerivatives end endDerivatives =
  bezier (Bezier.hermite start startDerivatives end endDerivatives)

synthetic :: Curve2d (space @ units) -> VectorCurve2d (space @ units) -> Curve2d (space @ units)
synthetic curve curveDerivative = new curve.compiled curveDerivative

instance HasField "startPoint" (Curve2d (space @ units)) (Point2d (space @ units)) where
  getField curve = evaluate curve 0.0

instance HasField "endPoint" (Curve2d (space @ units)) (Point2d (space @ units)) where
  getField curve = evaluate curve 1.0

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: Curve2d (space @ units) -> Bounds Unitless -> Bounds2d (space @ units)
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

samplePoints :: Curve2d (space @ units) -> List (Point2d (space @ units))
samplePoints curve = List.map (evaluate curve) Parameter.samples

-- | Reverse a curve, so that the start point is the end point and vice versa.
reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds curve = evaluateBounds curve Bounds.unitInterval

asPoint :: Tolerance units => Curve2d (space @ units) -> Maybe (Point2d (space @ units))
asPoint curve = do
  let testPoint = evaluate curve 0.5
  if List.allSatisfy (~= testPoint) (samplePoints curve) then Just testPoint else Nothing

tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (DirectionCurve2d space)
tangentDirection curve =
  case VectorCurve2d.direction curve.derivative of
    Success directionCurve -> Success directionCurve
    Failure VectorCurve2d.HasZero -> Failure HasDegeneracy

offsetLeftwardBy ::
  Tolerance units =>
  Qty units ->
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve2d (space @ units))
offsetLeftwardBy offset curve = Result.do
  tangentCurve <- tangentDirection curve
  let offsetCurve = VectorCurve2d.rotateBy Angle.quarterTurn (offset * tangentCurve)
  Success (curve + offsetCurve)

offsetRightwardBy ::
  Tolerance units =>
  Qty units ->
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve2d (space @ units))
offsetRightwardBy distance = offsetLeftwardBy -distance

distanceAlong :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve units
distanceAlong (Axis2d p0 d) curve = (curve - p0) `dot` d

distanceLeftOf :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve units
distanceLeftOf (Axis2d p0 d) curve = (curve - p0) `dot` Direction2d.rotateLeftward d

distanceRightOf :: Axis2d (space @ units) -> Curve2d (space @ units) -> Curve units
distanceRightOf (Axis2d p0 d) curve = (curve - p0) `dot` Direction2d.rotateRightward d

{-| Check if the given curve curve is collinear with (lies on) the given axis.

If the curve merely intersects/touches the axis at one or more points,
then it is not considered to lie on the axis;
it is only considered to lie on the axis if every point on the curve is also on the axis.
-}
isOnAxis :: Tolerance units => Axis2d (space @ units) -> Curve2d (space @ units) -> Bool
isOnAxis axis curve = List.allSatisfy (^ axis) (samplePoints curve)

-- | Get the X coordinate of a 2D curve as a scalar curve.
xCoordinate :: Curve2d (space @ units) -> Curve units
xCoordinate curve =
  Curve.new
    # CompiledFunction.map
      Expression.xCoordinate
      Point2d.xCoordinate
      Bounds2d.xCoordinate
      curve.compiled
    # curve.derivative.xComponent

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yCoordinate :: Curve2d (space @ units) -> Curve units
yCoordinate curve =
  Curve.new
    # CompiledFunction.map
      Expression.yCoordinate
      Point2d.yCoordinate
      Bounds2d.yCoordinate
      curve.compiled
    # curve.derivative.yComponent

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show, Error.Message)

findPoint ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Result IsCoincidentWithPoint (List Float)
findPoint point curve =
  case VectorCurve2d.zeros (point - curve) of
    Failure VectorCurve2d.ZeroEverywhere -> Failure IsCoincidentWithPoint
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
          OverlappingSegment (Bounds t1Start t1End) (Bounds t2Start t2End) $
            if (t1Start < t1End) == (t2Start < t2End) then Positive else Negative
      )
    |> List.filter (isOverlappingSegment curve1 curve2)

isOverlappingSegment ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  OverlappingSegment ->
  Bool
isOverlappingSegment curve1 curve2 (OverlappingSegment{t1}) = do
  let segmentStartPoint = evaluate curve1 (Bounds.lower t1)
  let curve1TestPoints = List.map (evaluate curve1) (Bounds.sampleValues t1)
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
    Failure IsCoincidentWithPoint -> Failure curveIsPointError

findEndpointIntersections ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Intersections.Error (List UvPoint)
findEndpointIntersections curve1 curve2 = Result.do
  start1Zeros <- findEndpointZeros curve1.startPoint curve2 Intersections.SecondCurveIsPoint
  end1Zeros <- findEndpointZeros curve1.endPoint curve2 Intersections.SecondCurveIsPoint
  start2Zeros <- findEndpointZeros curve2.startPoint curve1 Intersections.FirstCurveIsPoint
  end2Zeros <- findEndpointZeros curve2.endPoint curve1 Intersections.FirstCurveIsPoint
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
    [] ->
      if VectorCurve2d.hasZero curve1.derivative || VectorCurve2d.hasZero curve2.derivative
        then Failure Intersections.CurveHasDegeneracy
        else do
          let u = SurfaceFunction.u
          let v = SurfaceFunction.v
          let f = curve1 . u - curve2 . v
          let fu = VectorSurfaceFunction2d.derivative U f
          let fv = VectorSurfaceFunction2d.derivative V f
          let g = VectorSurfaceFunction2d.xy (fu `cross'` fv) ((curve1.derivative . u) `dot'` f)
          let gu = VectorSurfaceFunction2d.derivative U g
          let gv = VectorSurfaceFunction2d.derivative V g
          case Solve2d.search (findIntersectionPoints f fu fv g gu gv endpointIntersections) () of
            Success (NonEmpty points) -> Success (Just (IntersectionPoints points))
            Success [] -> Success Nothing
            Failure Solve2d.InfiniteRecursion -> exception "Higher-order intersection detected"
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
        case Bounds.resolvedSign (fvBounds `cross'` fuBounds) of
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
            case Bounds.resolvedSign (gvBounds `cross'` guBounds) of
              Resolved sign -> do
                case endpointIntersection endpointIntersections uvBounds of
                  Just point -> validate point IntersectionPoint.tangent sign
                  Nothing -> do
                    let gBounds = VectorSurfaceFunction2d.evaluateBounds g uvBounds
                    let convergenceTolerance =
                          1e-9 * Bounds.upper (VectorBounds2d.magnitude gBounds)
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
placeIn frame curve =
  new
    # CompiledFunction.map
      (Expression.Curve2d.placeIn frame)
      (Point2d.placeIn frame)
      (Bounds2d.placeIn frame)
      curve.compiled
    # VectorCurve2d.placeIn (Frame2d.orientation frame) curve.derivative

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Curve2d (global @ units) ->
  Curve2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

on ::
  Plane3d (space @ units) (Defines local) ->
  Curve2d (local @ units) ->
  Curve3d (space @ units)
on plane curve = Curve3d.on plane curve

transformBy ::
  Transform2d tag (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
transformBy transform curve =
  new
    # CompiledFunction.map
      (Expression.Curve2d.transformBy transform)
      (Point2d.transformBy transform)
      (Bounds2d.transformBy transform)
      curve.compiled
    # VectorCurve2d.transformBy transform curve.derivative

-- | Translate by the given displacement.
translateBy ::
  Vector2d (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

-- | Translate in the given direction by the given distance.
translateIn ::
  Direction2d space ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

-- | Translate along the given axis by the given distance.
translateAlong ::
  Axis2d (space @ units) ->
  Qty units ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

-- | Rotate around the given point by the given angle.
rotateAround ::
  Point2d (space @ units) ->
  Angle ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

-- | Mirror across the given axis.
mirrorAcross ::
  Axis2d (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

-- | Scale uniformly about the given point by the given scaling factor.
scaleAbout ::
  Point2d (space @ units) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAbout = Transform2d.scaleAboutImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong ::
  Axis2d (space @ units) ->
  Float ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
scaleAlong = Transform2d.scaleAlongImpl transformBy

convert ::
  Qty (units2 :/: units1) ->
  Curve2d (space @ units1) ->
  Curve2d (space @ units2)
convert factor curve = Units.coerce (scaleAbout Point2d.origin (Units.erase factor) curve)

unconvert ::
  Qty (units2 :/: units1) ->
  Curve2d (space @ units2) ->
  Curve2d (space @ units1)
unconvert factor curve = convert (1.0 /% factor) curve

curvature' ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve (Unitless :/: units))
curvature' curve = Result.do
  let firstDerivative = curve.derivative
  let secondDerivative = firstDerivative.derivative
  tangent <- tangentDirection curve
  Success ((tangent `cross` secondDerivative) !/!. (firstDerivative `dot'` firstDerivative))

curvature ::
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2d (space @ units1) ->
  Result HasDegeneracy (Curve units2)
curvature curve = Result.map Units.specialize (curvature' curve)

removeStartDegeneracy ::
  Int ->
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
removeStartDegeneracy continuity p1 d1 curve = Result.do
  let curveDerivatives = Stream.iterate (.derivative) curve.derivative
  let endDerivativeValues = Stream.map VectorCurve2d.endValue curveDerivatives
  let endCondition endDegree = (curve.endPoint, Stream.take endDegree endDerivativeValues)
  let baseCurve endDegree = do
        let (p2, d2) = endCondition endDegree
        hermite p1 d1 p2 d2
  let curveDerivative n =
        VectorCurve2d.synthetic
          (nthDerivative n (baseCurve (continuity + n)))
          (curveDerivative (n + 1))
  synthetic (baseCurve continuity) (curveDerivative 1)

nthDerivative :: Int -> Curve2d (space @ units) -> VectorCurve2d (space @ units)
nthDerivative 0 _ = internalError "nthDerivative should always be called with n >= 1"
nthDerivative 1 curve = curve.derivative
nthDerivative n curve = (nthDerivative (n - 1) curve).derivative

toPolyline :: Qty units -> Curve2d (space @ units) -> Polyline2d (Point2d (space @ units))
toPolyline accuracy curve =
  Polyline2d (NonEmpty.map (evaluate curve) (samplingPoints accuracy curve))

samplingPoints :: Qty units -> Curve2d (space @ units) -> NonEmpty Float
samplingPoints accuracy curve = do
  let secondDerivative = curve.derivative.derivative
  let predicate subdomain = do
        let secondDerivativeBounds = VectorCurve2d.evaluateBounds secondDerivative subdomain
        let secondDerivativeMagnitude = VectorBounds2d.magnitude secondDerivativeBounds
        Linearization.error secondDerivativeMagnitude subdomain <= accuracy
  Domain1d.samplingPoints predicate

medialAxis ::
  forall space units.
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result HasDegeneracy (List (MedialAxis.Segment (space @ units)))
medialAxis curve1 curve2 = do
  let p1 = curve1 . SurfaceFunction.u
  let p2 = curve2 . SurfaceFunction.v
  let v1 = curve1.derivative . SurfaceFunction.u
  let v2 = curve2.derivative . SurfaceFunction.v
  let d = p2 - p1
  let target = v2 `cross'` (2.0 * (v1 `dot'` d) .*. d - VectorSurfaceFunction2d.squaredMagnitude' d .*. v1)
  let targetTolerance = ?tolerance .*. ((?tolerance .*. ?tolerance) .*. ?tolerance)
  case Tolerance.using targetTolerance (SurfaceFunction.zeros target) of
    Failure SurfaceFunction.ZeroEverywhere -> TODO -- curves are identical arcs?
    Success zeros -> Result.do
      Debug.assert (List.isEmpty zeros.crossingLoops)
      Debug.assert (List.isEmpty zeros.tangentPoints)
      tangentDirection1 <- tangentDirection curve1
      let tangentVector1 = VectorCurve2d.unit tangentDirection1
      let normal1 = VectorCurve2d.rotateBy Angle.quarterTurn tangentVector1
      let radius :: SurfaceFunction units =
            (d `dot'` d) .!/! (2.0 * (tangentVector1 . SurfaceFunction.u) `cross` d)
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
      Success (List.map toSegment zeros.crossingCurves)

arcLengthParameterization ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (Curve Unitless, Qty units)
arcLengthParameterization curve = do
  if VectorCurve2d.isZero curve.derivative
    then Success (Curve.t, Qty.zero) -- Curve is a constant point
    else case VectorCurve2d.magnitude curve.derivative of
      Failure VectorCurve2d.HasZero -> Failure HasDegeneracy
      Success derivativeMagnitude -> Success (ArcLength.parameterization derivativeMagnitude)

unsafeArcLengthParameterization :: Curve2d (space @ units) -> (Curve Unitless, Qty units)
unsafeArcLengthParameterization curve =
  ArcLength.parameterization (VectorCurve2d.unsafeMagnitude curve.derivative)

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

makePiecewise :: NonEmpty (Curve2d (space @ units), Qty units) -> Curve2d (space @ units)
makePiecewise parameterizedSegments = do
  let segmentArray = Array.fromNonEmpty parameterizedSegments
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 segmentArray.length
  let evaluateImpl t = piecewiseValue tree (arcLength * t)
  let evaluateBoundsImpl (Bounds t1 t2) = piecewiseBounds tree (arcLength * t1) (arcLength * t2)
  new
    # CompiledFunction.abstract evaluateImpl evaluateBoundsImpl
    # piecewiseDerivative (piecewiseTreeDerivative tree arcLength) arcLength

piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d (space @ units)) ->
  Result HasDegeneracy (Curve2d (space @ units))
piecewise segments = Result.do
  parameterizedSegments <- Result.collect parameterizeByArcLength segments
  Success (makePiecewise parameterizedSegments)

unsafePiecewise :: NonEmpty (Curve2d (space @ units)) -> Curve2d (space @ units)
unsafePiecewise segments = makePiecewise (NonEmpty.map unsafeParameterizeByArcLength segments)

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

piecewiseValue :: PiecewiseTree (space @ units) -> Qty units -> Point2d (space @ units)
piecewiseValue tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseValue leftTree length
    | otherwise -> piecewiseValue rightTree (length - leftLength)
  PiecewiseLeaf curve segmentLength -> evaluate curve (length / segmentLength)

piecewiseBounds ::
  PiecewiseTree (space @ units) ->
  Qty units ->
  Qty units ->
  Bounds2d (space @ units)
piecewiseBounds tree startLength endLength = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseBounds rightTree (startLength - leftLength) (endLength - leftLength)
    | otherwise ->
        Bounds2d.aggregate2
          (piecewiseBounds leftTree startLength leftLength)
          (piecewiseBounds rightTree Qty.zero (endLength - leftLength))
  PiecewiseLeaf curve segmentLength ->
    evaluateBounds curve (Bounds (startLength / segmentLength) (endLength / segmentLength))

piecewiseDerivative ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  VectorCurve2d (space @ units)
piecewiseDerivative tree length = do
  let evaluateImpl t = piecewiseDerivativeValue tree (length * t)
  let evaluateBoundsImpl (Bounds t1 t2) = piecewiseDerivativeBounds tree (length * t1) (length * t2)
  VectorCurve2d.new
    (CompiledFunction.abstract evaluateImpl evaluateBoundsImpl)
    (piecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

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
    PiecewiseDerivativeLeaf ((length / segmentLength) * curve.derivative) segmentLength

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
      ((length / segmentLength) * curve.derivative)
      segmentLength

piecewiseDerivativeValue ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  Vector2d (space @ units)
piecewiseDerivativeValue tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseDerivativeValue leftTree length
    | otherwise -> piecewiseDerivativeValue rightTree (length - leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluate curve (length / segmentLength)

piecewiseDerivativeBounds ::
  PiecewiseDerivativeTree (space @ units) ->
  Qty units ->
  Qty units ->
  VectorBounds2d (space @ units)
piecewiseDerivativeBounds tree startLength endLength = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseDerivativeBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseDerivativeBounds
          rightTree
          (startLength - leftLength)
          (endLength - leftLength)
    | otherwise ->
        VectorBounds2d.aggregate2
          (piecewiseDerivativeBounds leftTree startLength leftLength)
          (piecewiseDerivativeBounds rightTree Qty.zero (endLength - leftLength))
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2d.evaluateBounds curve $
      Bounds (startLength / segmentLength) (endLength / segmentLength)
