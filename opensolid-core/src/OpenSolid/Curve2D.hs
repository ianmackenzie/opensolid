module OpenSolid.Curve2D
  ( Curve2D
  , Compiled
  , Segment
  , SearchTree
  , new
  , constant
  , xy
  , line
  , lineFrom
  , arc
  , arcFrom
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
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , desingularize
  , desingularized
  , point
  , startPoint
  , endPoint
  , endpoints
  , bounds
  , compiled
  , derivative
  , secondDerivative
  , tangentDirection
  , tangentDirectionBounds
  , curvatureVector_
  , curvatureVector
  , offsetLeftwardBy
  , offsetRightwardBy
  , reverse
  , overallBounds
  , g2
  , intersections
  , findPoint
  , distanceAlong
  , distanceLeftOf
  , distanceRightOf
  , isPoint
  , isOnAxis
  , xCoordinate
  , yCoordinate
  , coordinates
  , placeIn
  , relativeTo
  , placeOn
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
  , curvature_
  , toPolyline
  , medialAxis
  , arcLengthParameterizationFunction
  , arcLengthParameterization
  , parameterizeByArcLength
  , piecewise
  , searchTree
  )
where

import Data.Void (Void)
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2D (Arc2D)
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Axis2D (Axis2D (Axis2D))
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve2D, HasSingularity)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D.MedialAxis qualified as MedialAxis
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Line2D (Line2D)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Orientation2D qualified as Orientation2D
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polyline2D (Polyline2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Zeros qualified as SurfaceFunction1D.Zeros
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D

type Compiled units = Curve.Compiled 2 units Void

type Segment units = Curve.Segment 2 units Void

type SearchTree units = Curve.SearchTree 2 units Void

new :: Compiled units -> VectorCurve2D units -> Curve2D units
new = Curve.new

-- | Create a degenerate curve that is actually just a single point.
constant :: Point2D units -> Curve2D units
constant = Curve.constant

-- | Create a curve from its X and Y coordinate curves.
xy :: Curve1D units -> Curve1D units -> Curve2D units
xy x y = do
  let compiledX = Curve1D.compiled x
  let compiledY = Curve1D.compiled y
  let compiledXY = CompiledFunction.map2 Expression.xy Point2D Bounds2D compiledX compiledY
  let xyDerivative = VectorCurve2D.xy (Curve1D.derivative x) (Curve1D.derivative y)
  new compiledXY xyDerivative

-- | Convert a line to a curve.
line :: Line2D units -> Curve2D units
line = Curve.line

-- | Create a line between two points.
lineFrom :: Point2D units -> Point2D units -> Curve2D units
lineFrom = Curve.lineFrom

arc :: Arc2D units -> Curve2D units
arc givenArc =
  polarArc
    (#centerPoint (Arc2D.centerPoint givenArc))
    (#radius (Arc2D.radius givenArc))
    (#startAngle (Arc2D.startAngle givenArc))
    (#endAngle (Arc2D.endAngle givenArc))

{-| Create an arc from the given start point to the given end point, with the given swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
arcFrom :: Tolerance units => Point2D units -> Point2D units -> Angle -> Curve2D units
arcFrom givenStartPoint givenEndPoint sweptAngle =
  case Vector2D.magnitudeAndDirection (givenEndPoint - givenStartPoint) of
    Error Vector.IsZero -> lineFrom givenStartPoint givenEndPoint
    Ok (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 * distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 * sweptAngle)
      let linearDeviation = halfDistance * tanHalfAngle
      if linearDeviation ~= Quantity.zero
        then lineFrom givenStartPoint givenEndPoint
        else do
          let offset = (halfDistance / tanHalfAngle) * Direction2D.rotateLeft directionBetweenPoints
          let centerPoint = Point2D.midpoint givenStartPoint givenEndPoint + offset
          let radius = Point2D.distanceFrom centerPoint givenStartPoint
          let xVector = Vector2D.x radius
          let yVector = Vector2D.y radius
          let startAngle = Point2D.angleFrom centerPoint givenStartPoint
          let endAngle = startAngle + sweptAngle
          customArc centerPoint xVector yVector startAngle endAngle

-- | Create an arc with the given center point, radius, start angle and end angle.
polarArc ::
  ("centerPoint" ::: Point2D units) ->
  ("radius" ::: Quantity units) ->
  ("startAngle" ::: Angle) ->
  ("endAngle" ::: Angle) ->
  Curve2D units
polarArc
  ("centerPoint" ::: centerPoint)
  ("radius" ::: radius)
  ("startAngle" ::: startAngle)
  ("endAngle" ::: endAngle) =
    customArc centerPoint (Vector2D.x radius) (Vector2D.y radius) startAngle endAngle

{-| Create an arc with the given center point, start point and swept angle.

The start point will be swept around the center point by the given angle.
-}
sweptArc :: Point2D units -> Point2D units -> Angle -> Curve2D units
sweptArc centerPoint givenStartPoint sweptAngle = do
  let radius = Point2D.distanceFrom centerPoint givenStartPoint
  let startAngle = Point2D.angleFrom centerPoint givenStartPoint
  polarArc
    (#centerPoint centerPoint)
    (#radius radius)
    (#startAngle startAngle)
    (#endAngle (startAngle + sweptAngle))

-- | Create an arc for rounding off the corner between two straight lines.
cornerArc ::
  Tolerance units =>
  Point2D units ->
  "incoming" ::: Direction2D ->
  "outgoing" ::: Direction2D ->
  "radius" ::: Quantity units ->
  Curve2D units
cornerArc
  cornerPoint
  ("incoming" ::: incomingDirection)
  ("outgoing" ::: outgoingDirection)
  ("radius" ::: givenRadius) = do
    let radius = Quantity.abs givenRadius
    let sweptAngle = Direction2D.angleFrom incomingDirection outgoingDirection
    if 0.25 * radius * Number.squared (Angle.inRadians sweptAngle) ~= Quantity.zero
      then lineFrom cornerPoint cornerPoint
      else do
        let offset = radius * Number.abs (Angle.tan (0.5 * sweptAngle))
        let computedStartPoint = cornerPoint - offset * incomingDirection
        let computedEndPoint = cornerPoint + offset * outgoingDirection
        arcFrom computedStartPoint computedEndPoint sweptAngle

data WhichArc
  = SmallCounterclockwise
  | SmallClockwise
  | LargeCounterclockwise
  | LargeClockwise

radiusArc ::
  Tolerance units =>
  Quantity units ->
  Point2D units ->
  Point2D units ->
  WhichArc ->
  Curve2D units
radiusArc givenRadius givenStartPoint givenEndPoint whichArc =
  case Direction2D.from givenStartPoint givenEndPoint of
    Ok chordDirection -> do
      let halfDistance = 0.5 * Point2D.distanceFrom givenStartPoint givenEndPoint
      let radius = max (Quantity.abs givenRadius) halfDistance
      let offsetMagnitude =
            Quantity.sqrt_ (Quantity.squared_ radius - Quantity.squared_ halfDistance)
      let offsetDirection = Direction2D.rotateLeft chordDirection
      let offsetDistance =
            case whichArc of
              SmallCounterclockwise -> offsetMagnitude
              SmallClockwise -> -offsetMagnitude
              LargeClockwise -> offsetMagnitude
              LargeCounterclockwise -> -offsetMagnitude
      let offset = offsetDirection * offsetDistance
      let centerPoint = Point2D.midpoint givenStartPoint givenEndPoint + offset
      let shortAngle = 2.0 * Angle.asin (halfDistance / givenRadius)
      let sweptAngle =
            case whichArc of
              SmallCounterclockwise -> shortAngle
              SmallClockwise -> -shortAngle
              LargeClockwise -> shortAngle - Angle.twoPi
              LargeCounterclockwise -> Angle.twoPi - shortAngle
      sweptArc centerPoint givenStartPoint sweptAngle
    Error Direction2D.PointsAreCoincident ->
      lineFrom givenStartPoint givenEndPoint

ellipticalArc ::
  Frame2D units ->
  Quantity units ->
  Quantity units ->
  Angle ->
  Angle ->
  Curve2D units
ellipticalArc axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2D.originPoint axes
  let xVector = xRadius * Frame2D.xDirection axes
  let yVector = yRadius * Frame2D.yDirection axes
  customArc centerPoint xVector yVector startAngle endAngle

customArc ::
  Point2D units ->
  Vector2D units ->
  Vector2D units ->
  Angle ->
  Angle ->
  Curve2D units
customArc p0 v1 v2 a b = do
  let angle = Curve1D.interpolateFrom a b
  p0 + v1 * Curve1D.cos angle + v2 * Curve1D.sin angle

-- | Create a curve from the given circle.
circle :: Circle2D units -> Curve2D units
circle givenCircle =
  polarArc
    (#centerPoint (Circle2D.centerPoint givenCircle))
    (#radius (Circle2D.radius givenCircle))
    (#startAngle Angle.zero)
    (#endAngle Angle.twoPi)

{-| Create an ellipes with the given principal axes and major/minor radii.
The first radius given will be the radius along the X axis,
and the second radius will be the radius along the Y axis.
-}
ellipse :: Frame2D units -> Quantity units -> Quantity units -> Curve2D units
ellipse axes xRadius yRadius = ellipticalArc axes xRadius yRadius Angle.zero Angle.twoPi

{-| Construct a Bezier curve from its control points.

For example,

> Curve2D.bezier (NonEmpty.four p1 p2 p3 p4))

will return a cubic Bezier curve with the given four control points.
-}
bezier :: NonEmpty (Point2D units) -> Curve2D units
bezier = Curve.bezier

-- | Construct a quadratic Bezier curve from the given control points.
quadraticBezier ::
  Point2D units ->
  Point2D units ->
  Point2D units ->
  Curve2D units
quadraticBezier = Curve.quadraticBezier

-- | Construct a cubic Bezier curve from the given control points.
cubicBezier ::
  Point2D units ->
  Point2D units ->
  Point2D units ->
  Point2D units ->
  Curve2D units
cubicBezier = Curve.cubicBezier

{-| Construct a Bezier curve with the given endpoints and derivatives at those endpoints.

For example,

> Curve2D.hermite p1 [v1] p2 [v2]

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> Curve2D.hermite p1 [v1] p2 []

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  Point2D units ->
  List (Vector2D units) ->
  Point2D units ->
  List (Vector2D units) ->
  Curve2D units
hermite = Curve.hermite

{-# INLINE derivativeValue #-}
derivativeValue :: Curve2D units -> Number -> Vector2D units
derivativeValue = Curve.derivativeValue

{-# INLINE derivativeBounds #-}
derivativeBounds :: Curve2D units -> Interval Unitless -> VectorBounds2D units
derivativeBounds = Curve.derivativeBounds

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue :: Curve2D units -> Number -> Vector2D units
secondDerivativeValue = Curve.secondDerivativeValue

{-# INLINE secondDerivativeBounds #-}
secondDerivativeBounds :: Curve2D units -> Interval Unitless -> VectorBounds2D units
secondDerivativeBounds = Curve.secondDerivativeBounds

desingularize ::
  Maybe (Point2D units, Vector2D units) ->
  Curve2D units ->
  Maybe (Point2D units, Vector2D units) ->
  Curve2D units
desingularize = Curve.desingularize

desingularized :: Curve2D units -> Curve2D units -> Curve2D units -> Curve2D units
desingularized = Curve.desingularized

{-| Get the point on a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
point :: Curve2D units -> Number -> Point2D units
point = Curve.point

-- | Get the start point of a curve.
startPoint :: Curve2D units -> Point2D units
startPoint = Curve.startPoint

-- | Get the end point of a curve.
endPoint :: Curve2D units -> Point2D units
endPoint = Curve.endPoint

-- | Get the start and end points of a curve.
endpoints :: Curve2D units -> (Point2D units, Point2D units)
endpoints = Curve.endpoints

bounds :: Curve2D units -> Interval Unitless -> Bounds2D units
bounds = Curve.bounds

-- | Reverse a curve, so that the start point is the end point and vice versa.
reverse :: Curve2D units -> Curve2D units
reverse = Curve.reverse

overallBounds :: Curve2D units -> Bounds2D units
overallBounds = Curve.overallBounds

compiled :: Curve2D units -> Compiled units
compiled = Curve.compiled

derivative :: Curve2D units -> VectorCurve2D units
derivative = Curve.derivative

secondDerivative :: Curve2D units -> VectorCurve2D units
secondDerivative = Curve.secondDerivative

tangentDirection :: Tolerance units => Curve2D units -> Result IsDegenerate DirectionCurve2D
tangentDirection = Curve.tangentDirection

tangentDirectionBounds :: Curve2D units -> Interval Unitless -> DirectionBounds2D
tangentDirectionBounds = Curve.tangentDirectionBounds

curvatureVector_ ::
  Tolerance units =>
  Curve2D units ->
  Result HasSingularity (VectorCurve2D (Unitless ?/? units))
curvatureVector_ = Curve.curvatureVector_

curvatureVector ::
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2D units1 ->
  Result HasSingularity (VectorCurve2D units2)
curvatureVector curve = Result.map Units.specialize (curvatureVector_ curve)

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2D units ->
  Result IsDegenerate (Curve2D units)
offsetLeftwardBy offset curve = do
  tangentCurve <- tangentDirection curve
  let offsetCurve = VectorCurve2D.rotateBy Angle.quarterTurn (offset * tangentCurve)
  Ok (curve + offsetCurve)

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Curve2D units ->
  Result IsDegenerate (Curve2D units)
offsetRightwardBy distance = offsetLeftwardBy -distance

distanceAlong :: Axis2D units -> Curve2D units -> Curve1D units
distanceAlong (Axis2D p0 d) curve = (curve - p0) `dot` d

distanceLeftOf :: Axis2D units -> Curve2D units -> Curve1D units
distanceLeftOf (Axis2D p0 d) curve = (curve - p0) `dot` Direction2D.rotateLeft d

distanceRightOf :: Axis2D units -> Curve2D units -> Curve1D units
distanceRightOf (Axis2D p0 d) curve = (curve - p0) `dot` Direction2D.rotateRight d

isPoint :: Tolerance units => Curve2D units -> Bool
isPoint = Curve.isPoint

{-| Check if the given curve curve is collinear with (lies on) the given axis.

If the curve merely intersects/touches the axis at one or more points,
then it is not considered to lie on the axis;
it is only considered to lie on the axis if every point on the curve is also on the axis.
-}
isOnAxis :: Tolerance units => Axis2D units -> Curve2D units -> Bool
isOnAxis = Curve.isOnAxis

-- | Get the X coordinate of a 2D curve as a scalar curve.
xCoordinate :: Curve2D units -> Curve1D units
xCoordinate curve = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2D.xCoordinate
          Bounds2D.xCoordinate
          (compiled curve)
  Curve1D.new compiledXCoordinate (VectorCurve2D.xComponent (derivative curve))

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yCoordinate :: Curve2D units -> Curve1D units
yCoordinate curve = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2D.yCoordinate
          Bounds2D.yCoordinate
          (compiled curve)
  Curve1D.new compiledYCoordinate (VectorCurve2D.yComponent (derivative curve))

coordinates :: Curve2D units -> (Curve1D units, Curve1D units)
coordinates curve = (xCoordinate curve, yCoordinate curve)

findPoint :: Tolerance units => Point2D units -> Curve2D units -> List Number
findPoint = Curve.findPoint

intersections ::
  Tolerance units =>
  Curve2D units ->
  Curve2D units ->
  Result IsDegenerate (Maybe Curve.Intersections)
intersections = Curve.intersections

g2 ::
  Tolerance units =>
  (Curve2D units, Number) ->
  (Curve2D units, Number) ->
  Quantity units ->
  Bool
g2 (curve1, t1) (curve2, t2) radius =
  point curve1 t1 ~= point curve2 t2 && do
    let Vector2D dxdt1 dydt1 = derivativeValue curve1 t1
    let Vector2D dxdt2 dydt2 = derivativeValue curve2 t2
    let dxdtMin = min (Quantity.abs dxdt1) (Quantity.abs dxdt2)
    let dydtMin = min (Quantity.abs dydt1) (Quantity.abs dydt2)
    let orientation =
          if dxdtMin >= dydtMin
            then Orientation2D.horizontal
            else Orientation2D.vertical
    let signature1 = signature orientation curve1 t1 radius
    let signature2 = signature orientation curve2 t2 radius
    signature1 ~= signature2

signature ::
  Tolerance units =>
  Orientation2D ->
  Curve2D units ->
  Number ->
  Quantity units ->
  (Quantity units, Quantity units)
signature orientation curve tValue radius = do
  let local vector = Vector2D.relativeToOrientation orientation vector
  let Vector2D x' y' = local (derivativeValue curve tValue)
  let Vector2D x'' y'' = local (secondDerivativeValue curve tValue)
  let dydx = if x' != Quantity.zero then y' / x' else y'' / x''
  let firstOrder = dydx * radius
  let d2ydx2 =
        if x' != Quantity.zero
          then (y'' ?*? x' - y' ?*? x'') ?/? (x' ?*? x' ?*? x')
          else do
            let fourthDerivative =
                  curve
                    & derivative
                    & VectorCurve2D.derivative
                    & VectorCurve2D.derivative
                    & VectorCurve2D.derivative
            let Vector2D x'''' y'''' = local (VectorCurve2D.value fourthDerivative tValue)
            (y'''' ?*? x'' - y'' ?*? x'''') ?/? (x'' ?*? x'' ?*? x'')
  let secondOrder = Units.simplify (0.5 * d2ydx2 ?*? Quantity.squared_ radius)
  (firstOrder, secondOrder)

placeIn :: Frame2D units -> Curve2D units -> Curve2D units
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point2D.placeIn frame)
          (Bounds2D.placeIn frame)
          (compiled curve)
  new compiledPlaced (VectorCurve2D.placeIn frame (derivative curve))

relativeTo :: Frame2D units -> Curve2D units -> Curve2D units
relativeTo frame = placeIn (Frame2D.inverse frame)

placeOn :: Plane3D space -> Curve2D Meters -> Curve3D space
placeOn plane curve = Curve3D.on plane curve

transformBy :: Transform2D tag units -> Curve2D units -> Curve2D units
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point2D.transformBy transform)
          (Bounds2D.transformBy transform)
          (compiled curve)
  new compiledTransformed (VectorCurve2D.transformBy transform (derivative curve))

-- | Translate by the given displacement.
translateBy :: Vector2D units -> Curve2D units -> Curve2D units
translateBy = Transform2D.translateByImpl transformBy

-- | Translate in the given direction by the given distance.
translateIn :: Direction2D -> Quantity units -> Curve2D units -> Curve2D units
translateIn = Transform2D.translateInImpl transformBy

-- | Translate along the given axis by the given distance.
translateAlong :: Axis2D units -> Quantity units -> Curve2D units -> Curve2D units
translateAlong = Transform2D.translateAlongImpl transformBy

-- | Rotate around the given point by the given angle.
rotateAround :: Point2D units -> Angle -> Curve2D units -> Curve2D units
rotateAround = Transform2D.rotateAroundImpl transformBy

-- | Mirror across the given axis.
mirrorAcross :: Axis2D units -> Curve2D units -> Curve2D units
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy

-- | Scale uniformly about the given point by the given scaling factor.
scaleAbout :: Point2D units -> Number -> Curve2D units -> Curve2D units
scaleAbout = Transform2D.scaleAboutImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong :: Axis2D units -> Number -> Curve2D units -> Curve2D units
scaleAlong = Transform2D.scaleAlongImpl transformBy

convert :: Quantity (units2 ?/? units1) -> Curve2D units1 -> Curve2D units2
convert factor curve = Units.coerce (scaleAbout Point2D.origin (Units.erase factor) curve)

unconvert :: Quantity (units2 ?/? units1) -> Curve2D units2 -> Curve2D units1
unconvert factor curve = convert (Units.simplify (1.0 ?/? factor)) curve

curvature_ ::
  Tolerance units =>
  Curve2D units ->
  Result IsDegenerate (Curve1D (Unitless ?/? units))
curvature_ curve = do
  tangent <- tangentDirection curve
  let numerator = tangent `cross` secondDerivative curve
  let denominator = VectorCurve2D.squaredMagnitude_ (derivative curve)
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.quotient_ numerator denominator) of
    Ok quotient_ -> Ok (Units.simplify quotient_)
    Error DivisionByZero -> Error IsDegenerate

{-| Get the curvature of a 2D curve.

This is the inverse of the radius of curvature, but is in general a better-defined quantity
since the radius of curvature can go to infinity if the curve has zero curvature anywhere,
and can in fact go through a singularity where it flips from positive to negative infinity
if the curve has an inflection point where curvature goes from positive to zero to negative.

Positive curvature is defined as curving to the left (relative to the curve's tangent direction).
-}
curvature ::
  (Tolerance units1, Units.Inverse units1 units2) =>
  Curve2D units1 ->
  Result IsDegenerate (Curve1D units2)
curvature curve = Result.map Units.specialize (curvature_ curve)

toPolyline :: Resolution units -> Curve2D units -> Polyline2D units
toPolyline = Curve.toPolyline

medialAxis ::
  forall units.
  Tolerance units =>
  Curve2D units ->
  Curve2D units ->
  Result IsDegenerate (List (MedialAxis.Segment units))
medialAxis curve1 curve2 = do
  let p1 = curve1 . SurfaceFunction1D.u
  let p2 = curve2 . SurfaceFunction1D.v
  let v1 = derivative curve1 . SurfaceFunction1D.u
  let v2 = derivative curve2 . SurfaceFunction1D.v
  let d = p2 - p1
  let target =
        v2 `cross_` (2.0 * (v1 `dot_` d) ?*? d - VectorSurfaceFunction2D.squaredMagnitude_ d ?*? v1)
  let targetTolerance = ?tolerance ?*? ((?tolerance ?*? ?tolerance) ?*? ?tolerance)
  case Tolerance.using targetTolerance (SurfaceFunction1D.zeros target) of
    Error SurfaceFunction1D.IsZero -> TODO -- curves are identical arcs?
    Ok zeros ->
      assert (List.isEmpty zeros.crossingLoops && List.isEmpty zeros.tangentPoints) do
        tangentDirection1 <- tangentDirection curve1
        let tangentVector1 = VectorCurve2D.unit tangentDirection1
        let normal1 = VectorCurve2D.rotateBy Angle.quarterTurn tangentVector1
        let radius :: SurfaceFunction1D units =
              Units.coerce $
                SurfaceFunction1D.unsafeQuotient_
                  (d `dot_` d)
                  (2.0 * (tangentVector1 . SurfaceFunction1D.u) `cross` d)
        let curve :: SurfaceFunction2D units =
              (curve1 . SurfaceFunction1D.u) + radius * (normal1 . SurfaceFunction1D.u)
        let toSegment solutionCurve =
              MedialAxis.Segment
                { t1 = xCoordinate solutionCurve
                , t2 = yCoordinate solutionCurve
                , t12 = solutionCurve
                , curve = curve . solutionCurve
                , radius = radius . solutionCurve
                }
        Ok (List.map toSegment zeros.crossingCurves)

arcLengthParameterizationFunction ::
  Tolerance units =>
  Curve2D units ->
  (Number -> Number, Quantity units)
arcLengthParameterizationFunction = Curve.arcLengthParameterizationFunction

arcLengthParameterization :: Tolerance units => Curve2D units -> (Curve1D Unitless, Quantity units)
arcLengthParameterization = Curve.arcLengthParameterization

parameterizeByArcLength :: Tolerance units => Curve2D units -> (Curve2D units, Quantity units)
parameterizeByArcLength = Curve.parameterizeByArcLength

makePiecewise :: NonEmpty (Curve2D units, Quantity units) -> Curve2D units
makePiecewise parameterizedSegments = do
  let segmentArray = Array.fromNonEmpty parameterizedSegments
  let (tree, arcLength) = buildPiecewiseTree segmentArray 0 (Array.length segmentArray)
  let pointImpl t = piecewisePoint tree (arcLength * t)
  let boundsImpl (Interval t1 t2) = piecewiseBounds tree (arcLength * t1) (arcLength * t2)
  new
    (CompiledFunction.abstract pointImpl boundsImpl)
    (piecewiseDerivative (piecewiseTreeDerivative tree arcLength) arcLength)

piecewise :: Tolerance units => NonEmpty (Curve2D units) -> Curve2D units
piecewise segments = makePiecewise (NonEmpty.map parameterizeByArcLength segments)

buildPiecewiseTree ::
  Array (Curve2D units, Quantity units) ->
  Int ->
  Int ->
  (PiecewiseTree units space, Quantity units)
buildPiecewiseTree segmentArray begin end = case end - begin of
  1 -> do
    let (segment, length) = segmentArray !! begin
    (PiecewiseLeaf segment length, length)
  n -> assert (n >= 2) do
    let mid = begin + n // 2
    let (leftTree, leftLength) = buildPiecewiseTree segmentArray begin mid
    let (rightTree, rightLength) = buildPiecewiseTree segmentArray mid end
    (PiecewiseNode leftTree leftLength rightTree, leftLength + rightLength)

data PiecewiseTree units space where
  PiecewiseNode ::
    PiecewiseTree units space ->
    Quantity units ->
    PiecewiseTree units space ->
    PiecewiseTree units space
  PiecewiseLeaf ::
    Curve2D units ->
    Quantity units ->
    PiecewiseTree units space

piecewisePoint :: PiecewiseTree units space -> Quantity units -> Point2D units
piecewisePoint tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | length < leftLength -> piecewisePoint leftTree length
    | otherwise -> piecewisePoint rightTree (length - leftLength)
  PiecewiseLeaf curve segmentLength -> point curve (length / segmentLength)

piecewiseBounds ::
  PiecewiseTree units space ->
  Quantity units ->
  Quantity units ->
  Bounds2D units
piecewiseBounds tree startLength endLength = case tree of
  PiecewiseNode leftTree leftLength rightTree
    | endLength <= leftLength ->
        piecewiseBounds leftTree startLength endLength
    | startLength >= leftLength ->
        piecewiseBounds rightTree (startLength - leftLength) (endLength - leftLength)
    | otherwise ->
        Bounds2D.aggregate2
          (piecewiseBounds leftTree startLength leftLength)
          (piecewiseBounds rightTree Quantity.zero (endLength - leftLength))
  PiecewiseLeaf curve segmentLength ->
    bounds curve (Interval (startLength / segmentLength) (endLength / segmentLength))

piecewiseDerivative ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  VectorCurve2D units
piecewiseDerivative tree length = do
  let valueImpl t = piecewiseDerivativeValue tree (length * t)
  let boundsImpl (Interval t1 t2) = piecewiseDerivativeBounds tree (length * t1) (length * t2)
  VectorCurve2D.new
    (CompiledFunction.abstract valueImpl boundsImpl)
    (piecewiseDerivative (piecewiseDerivativeTreeDerivative tree length) length)

data PiecewiseDerivativeTree units space where
  PiecewiseDerivativeNode ::
    PiecewiseDerivativeTree units space ->
    Quantity units ->
    PiecewiseDerivativeTree units space ->
    PiecewiseDerivativeTree units space
  PiecewiseDerivativeLeaf ::
    VectorCurve2D units ->
    Quantity units ->
    PiecewiseDerivativeTree units space

piecewiseTreeDerivative ::
  PiecewiseTree units space ->
  Quantity units ->
  PiecewiseDerivativeTree units space
piecewiseTreeDerivative tree length = case tree of
  PiecewiseNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseTreeDerivative leftTree length)
      leftLength
      (piecewiseTreeDerivative rightTree length)
  PiecewiseLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf ((length / segmentLength) * derivative curve) segmentLength

piecewiseDerivativeTreeDerivative ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  PiecewiseDerivativeTree units space
piecewiseDerivativeTreeDerivative tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree ->
    PiecewiseDerivativeNode
      (piecewiseDerivativeTreeDerivative leftTree length)
      leftLength
      (piecewiseDerivativeTreeDerivative rightTree length)
  PiecewiseDerivativeLeaf curve segmentLength ->
    PiecewiseDerivativeLeaf
      ((length / segmentLength) * VectorCurve2D.derivative curve)
      segmentLength

piecewiseDerivativeValue ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Vector2D units
piecewiseDerivativeValue tree length = case tree of
  PiecewiseDerivativeNode leftTree leftLength rightTree
    | length < leftLength -> piecewiseDerivativeValue leftTree length
    | otherwise -> piecewiseDerivativeValue rightTree (length - leftLength)
  PiecewiseDerivativeLeaf curve segmentLength ->
    VectorCurve2D.value curve (length / segmentLength)

piecewiseDerivativeBounds ::
  PiecewiseDerivativeTree units space ->
  Quantity units ->
  Quantity units ->
  VectorBounds2D units
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
        VectorBounds2D.aggregate2
          (piecewiseDerivativeBounds leftTree startLength leftLength)
          (piecewiseDerivativeBounds rightTree Quantity.zero (endLength - leftLength))
  PiecewiseDerivativeLeaf curve segmentLength -> do
    let tBounds = Interval (startLength / segmentLength) (endLength / segmentLength)
    VectorCurve2D.bounds curve tBounds

searchTree :: Curve2D units -> SearchTree units
searchTree = Curve.searchTree
