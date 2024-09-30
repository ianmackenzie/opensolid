module Arc2d
  ( from
  , polar
  , circle
  , swept
  , corner
  , withRadius
  , Direction
  , Size
  , counterclockwise
  , clockwise
  , small
  , large
  , elliptical
  , ellipse
  , generic
  , Arc2d
  , centerPoint
  , majorDirection
  , minorDirection
  , majorRadius
  , minorRadius
  , startAngle
  , endAngle
  , placeIn
  )
where

import Angle qualified
import Arithmetic.Unboxed
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d (Direction2d)
import Direction2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Jit.Expression qualified as Expression
import Line2d qualified
import Maybe qualified
import OpenSolid
import Point2d (Point2d (Point2d#))
import Point2d qualified
import Qty (Qty (Qty#))
import Qty qualified
import Range qualified
import Units qualified
import Vector2d (Vector2d (Vector2d#))
import Vector2d qualified
import VectorCurve2d qualified

data Arc (coordinateSystem :: CoordinateSystem) where
  Arc ::
    Point2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Angle ->
    Angle ->
    Arc (space @ units)

deriving instance Show (Arc (space @ units))

deriving instance Eq (Arc (space @ units))

instance Curve2d.Interface (Arc (space @ units)) (space @ units) where
  startPointImpl arc = Curve2d.pointOnImpl arc 0.0
  endPointImpl arc = Curve2d.pointOnImpl arc 1.0
  pointOnImpl (Arc p0 vx vy (Qty# a#) (Qty# b#)) (Qty# t#) = do
    let !(Point2d# x0# y0#) = p0
    let !(Vector2d# x1# y1#) = vx
    let !(Vector2d# x2# y2#) = vy
    let theta# = a# +# t# *# (b# -# a#)
    let cosTheta# = cos# theta#
    let sinTheta# = sin# theta#
    let x# = x0# +# cosTheta# *# x1# +# sinTheta# *# x2#
    let y# = y0# +# cosTheta# *# y1# +# sinTheta# *# y2#
    Point2d# x# y#
  segmentBoundsImpl (Arc p0 vx vy a b) t = do
    let theta = a + (b - a) * t
    p0 + Range.cos theta * vx + Range.sin theta * vy
  derivativeImpl (Arc _ vx vy a b) = VectorCurve2d.derivative (VectorCurve2d.arc vx vy a b)
  reverseImpl (Arc p0 vx vy a b) = Arc p0 vx vy b a
  boundsImpl arc = Curve2d.segmentBoundsImpl arc Range.unit
  transformByImpl transform (Arc p0 vx vy a b) =
    Curve2d.new $
      Arc
        (Point2d.transformBy transform p0)
        (Vector2d.transformBy transform vx)
        (Vector2d.transformBy transform vy)
        a
        b
  asArcImpl (Arc centerPoint vx vy a b) = Maybe.do
    let theta = 0.5 * Angle.atan2 (2 * vx .<>. vy) (vx .<>. vx - vy .<>. vy)
    let cosTheta = Angle.cos theta
    let sinTheta = Angle.sin theta
    let v1 = vx * cosTheta + vy * sinTheta
    let v2 = vy * cosTheta - vx * sinTheta
    (r1, d1) <- maybeMagnitudeAndDirection v1
    (r2, d2) <- maybeMagnitudeAndDirection v2
    if r1 >= r2
      then do
        let startAngle = a - theta
        let endAngle = b - theta
        Just (Arc2d centerPoint d1 d2 r1 r2 startAngle endAngle)
      else do
        let startAngle = a - theta - Angle.quarterTurn
        let endAngle = b - theta - Angle.quarterTurn
        Just (Arc2d centerPoint d2 -d1 r2 r1 startAngle endAngle)
  toAstImpl (Arc p0 vx vy a b) = do
    let angle = a + Expression.parameter * (b - a)
    Just (p0 + vx * Expression.cos angle + vy * Expression.sin angle)

maybeMagnitudeAndDirection :: Tolerance units => Vector2d (space @ units) -> Maybe (Qty units, Direction2d space)
maybeMagnitudeAndDirection vector =
  case Vector2d.magnitudeAndDirection vector of
    Success magnitudeAndDirection -> Just magnitudeAndDirection
    Failure Vector2d.IsZero -> Nothing

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Angle ->
  Curve2d (space @ units)
from startPoint endPoint sweptAngle =
  case Vector2d.magnitudeAndDirection (endPoint - startPoint) of
    Failure Vector2d.IsZero -> Line2d.from startPoint endPoint
    Success (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 * distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 * sweptAngle)
      let linearDeviation = halfDistance * tanHalfAngle
      if linearDeviation ~= Qty.zero
        then Line2d.from startPoint endPoint
        else do
          let offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeft directionBetweenPoints
          let centerPoint = Point2d.midpoint startPoint endPoint + offset
          let radius = Point2d.distanceFrom centerPoint startPoint
          let xVector = Vector2d.x radius
          let yVector = Vector2d.y radius
          let startAngle = Point2d.angleFrom centerPoint startPoint
          let endAngle = startAngle + sweptAngle
          Curve2d.new (Arc centerPoint xVector yVector startAngle endAngle)

polar :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
polar centerPoint radius startAngle endAngle =
  Curve2d.new (Arc centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle)

circle :: Point2d (space @ units) -> Qty units -> Curve2d (space @ units)
circle centerPoint radius = polar centerPoint radius Angle.zero Angle.twoPi

swept :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Curve2d (space @ units)
swept centerPoint startPoint sweptAngle = do
  let radius = Point2d.distanceFrom centerPoint startPoint
  let startAngle = Point2d.angleFrom centerPoint startPoint
  polar centerPoint radius startAngle (startAngle + sweptAngle)

corner ::
  Tolerance units =>
  Point2d (space @ units) ->
  Direction2d space ->
  Direction2d space ->
  Qty units ->
  Curve2d (space @ units)
corner cornerPoint incomingDirection outgoingDirection givenRadius = do
  let radius = Qty.abs givenRadius
  let sweptAngle = Direction2d.angleFrom incomingDirection outgoingDirection
  if radius * Float.squared (Angle.inRadians sweptAngle) / 4 ~= Qty.zero
    then Line2d.from cornerPoint cornerPoint
    else do
      let offset = radius * Qty.abs (Angle.tan (0.5 * sweptAngle))
      let startPoint = cornerPoint - offset * incomingDirection
      let endPoint = cornerPoint + offset * outgoingDirection
      from startPoint endPoint sweptAngle

withRadius ::
  Qty units ->
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Direction ->
  Size ->
  Curve2d (space @ units)
withRadius givenRadius startPoint endPoint direction size =
  case Direction2d.from startPoint endPoint of
    Success chordDirection -> do
      let halfDistance = 0.5 * Point2d.distanceFrom startPoint endPoint
      let radius = Qty.max (Qty.abs givenRadius) halfDistance
      let offsetMagnitude = Qty.sqrt' (Qty.squared' radius - Qty.squared' halfDistance)
      let offsetDirection = Direction2d.rotateLeft chordDirection
      let offsetDistance =
            case (direction, size) of
              (Counterclockwise, Small) -> offsetMagnitude
              (Clockwise, Small) -> -offsetMagnitude
              (Clockwise, Large) -> offsetMagnitude
              (Counterclockwise, Large) -> -offsetMagnitude
      let offset = offsetDirection * offsetDistance
      let centerPoint = Point2d.midpoint startPoint endPoint + offset
      let shortAngle = 2 * Angle.asin (halfDistance / givenRadius)
      let sweptAngle =
            case (direction, size) of
              (Counterclockwise, Small) -> shortAngle
              (Clockwise, Small) -> -shortAngle
              (Clockwise, Large) -> shortAngle - Angle.fullTurn
              (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
      swept centerPoint startPoint sweptAngle
    Failure Direction2d.PointsAreCoincident ->
      Line2d.from startPoint endPoint

data Direction = Clockwise | Counterclockwise

data Size = Large | Small

counterclockwise :: Direction
counterclockwise = Counterclockwise

clockwise :: Direction
clockwise = Clockwise

small :: Size
small = Small

large :: Size
large = Large

elliptical ::
  Frame2d (space @ units) defines ->
  Qty units ->
  Qty units ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
elliptical axes xRadius yRadius startAngle endAngle = do
  let centerPoint = Frame2d.originPoint axes
  let xVector = xRadius * Frame2d.xDirection axes
  let yVector = yRadius * Frame2d.yDirection axes
  generic centerPoint xVector yVector startAngle endAngle

ellipse :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Curve2d (space @ units)
ellipse axes xRadius yRadius = elliptical axes xRadius yRadius Angle.zero Angle.twoPi

generic ::
  Point2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
generic p0 v1 v2 a b = Curve2d.new (Arc p0 v1 v2 a b)

type role Arc2d nominal

data Arc2d (coordinateSystem :: CoordinateSystem) where
  Arc2d ::
    { centerPoint :: Point2d (space @ units)
    , majorDirection :: Direction2d space
    , minorDirection :: Direction2d space
    , majorRadius :: Qty units
    , minorRadius :: Qty units
    , startAngle :: Angle
    , endAngle :: Angle
    } ->
    Arc2d (space @ units)

instance HasUnits (Arc2d (space @ units)) where
  type UnitsOf (Arc2d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Arc2d (space1 @ unitsA)) (Arc2d (space2 @ unitsB))
  where
  coerce arc =
    arc
      { centerPoint = Units.coerce (centerPoint arc)
      , majorRadius = Units.coerce (majorRadius arc)
      , minorRadius = Units.coerce (minorRadius arc)
      }

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Arc2d (local @ units) ->
  Arc2d (global @ units)
placeIn frame arc =
  arc
    { centerPoint = Point2d.placeIn frame (centerPoint arc)
    , majorDirection = Direction2d.placeIn frame (majorDirection arc)
    , minorDirection = Direction2d.placeIn frame (minorDirection arc)
    }
