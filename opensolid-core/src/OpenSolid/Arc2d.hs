module OpenSolid.Arc2d
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
  , new
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d

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
          new centerPoint xVector yVector startAngle endAngle

polar :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
polar centerPoint radius startAngle endAngle =
  new centerPoint (Vector2d.x radius) (Vector2d.y radius) startAngle endAngle

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
  if radius * Float.squared (Angle.inRadians sweptAngle) / 4.0 ~= Qty.zero
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
      let shortAngle = 2.0 * Angle.asin (halfDistance / givenRadius)
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
  new centerPoint xVector yVector startAngle endAngle

ellipse :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Curve2d (space @ units)
ellipse axes xRadius yRadius = elliptical axes xRadius yRadius Angle.zero Angle.twoPi

new ::
  Point2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
new p0 v1 v2 a b = do
  let angle = Expression.Curve1d.constant a + Expression.t * Expression.Curve1d.constant (b - a)
  Curve2d.Parametric $
    Expression.Curve2d.constant p0
      + Expression.VectorCurve2d.constant v1 * Expression.cos angle
      + Expression.VectorCurve2d.constant v2 * Expression.sin angle
