module Arc2d
  ( from
  , with
  , Constraint (..)
  , BuildError
  , Direction (Clockwise, Counterclockwise)
  , Size (Large, Small)
  )
where

import Angle (Angle)
import Angle qualified
import CoordinateSystem (Units)
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Result qualified
import Units qualified
import Vector2d qualified

data Constraint coordinateSystem
  = CenterPoint (Point2d coordinateSystem)
  | StartPoint (Point2d coordinateSystem)
  | EndPoint (Point2d coordinateSystem)
  | Radius (Qty (Units coordinateSystem))
  | StartAngle Angle
  | EndAngle Angle
  | SweptAngle Angle
  | Direction Direction
  | Size Size
  deriving (Eq, Ord)

data Direction = Clockwise | Counterclockwise deriving (Eq, Ord)

data Size = Large | Small deriving (Eq, Ord)

data BuildError
  = UnsupportedConstraints
  | EndpointsAreCoincident
  | EndpointsAreTooFarApart
  deriving (Eq, Show, ErrorMessage)

with :: List (Constraint (space @ units)) -> Result BuildError (Curve2d (space @ units))
with constraints = case List.sort constraints of
  [CenterPoint p0, Radius r, StartAngle theta1, EndAngle theta2] ->
    Ok $ fromCenterPointRadiusStartAngleEndAngle p0 r theta1 theta2
  [CenterPoint p0, Radius r, StartAngle theta1, SweptAngle theta] ->
    Ok $ fromCenterPointRadiusStartAngleEndAngle p0 r theta1 (theta1 + theta)
  [CenterPoint p0, StartPoint p1, SweptAngle theta] ->
    Ok $ fromCenterPointStartPointSweptAngle p0 p1 theta
  [StartPoint p1, EndPoint p2, Radius r, Direction d, Size s] ->
    fromStartPointEndPointRadiusDirectionSize p1 p2 r d s
  [StartPoint p1, EndPoint p2, SweptAngle theta] ->
    Ok (from p1 p2 theta)
  _ -> Error UnsupportedConstraints

fromCenterPointRadiusStartAngleEndAngle :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
fromCenterPointRadiusStartAngleEndAngle = Curve2d.Arc

fromCenterPointStartPointSweptAngle :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Curve2d (space @ units)
fromCenterPointStartPointSweptAngle p0 p1 theta =
  let computedStartAngle = Point2d.angleFrom p0 p1
   in Curve2d.Arc p0 (Point2d.distanceFrom p0 p1) computedStartAngle (computedStartAngle + theta)

fromStartPointEndPointRadiusDirectionSize
  :: Point2d (space @ units)
  -> Point2d (space @ units)
  -> Qty units
  -> Direction
  -> Size
  -> Result BuildError (Curve2d (space @ units))
fromStartPointEndPointRadiusDirectionSize givenStartPoint givenEndPoint givenRadius direction size = do
  chordDirection <-
    Direction2d.from givenStartPoint givenEndPoint
      |> Result.mapError (\Direction2d.PointsAreCoincident -> Arc2d.EndpointsAreCoincident)
  let squaredRadius = Qty.squared (Units.generalize givenRadius)
  let squaredHalfLength = Qty.squared (Units.generalize (0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint))
  squaredOffsetMagnitude <-
    Qty.nonNegative (squaredRadius - squaredHalfLength)
      |> Result.mapError (\Qty.IsNegative -> Arc2d.EndpointsAreTooFarApart)
  let offsetMagnitude = Units.specialize (Qty.sqrt squaredOffsetMagnitude)
  let offsetDirection = Direction2d.perpendicularTo chordDirection
  let offsetDistance =
        case (direction, size) of
          (Counterclockwise, Small) -> offsetMagnitude
          (Clockwise, Small) -> -offsetMagnitude
          (Clockwise, Large) -> offsetMagnitude
          (Counterclockwise, Large) -> -offsetMagnitude
  let computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offsetDirection * offsetDistance
  let halfLength = Units.specialize (Qty.sqrt squaredHalfLength)
  let shortAngle = 2.0 * Angle.asin (halfLength / givenRadius)
  let computedSweptAngle =
        case (direction, size) of
          (Counterclockwise, Small) -> shortAngle
          (Clockwise, Small) -> -shortAngle
          (Clockwise, Large) -> shortAngle - Angle.fullTurn
          (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
  Ok (fromCenterPointStartPointSweptAngle computedCenterPoint givenStartPoint computedSweptAngle)

from :: Point2d (space @ units) -> Point2d (space @ units) -> Angle -> Curve2d (space @ units)
from p1 p2 theta =
  case Vector2d.magnitudeAndDirection (p2 - p1) of
    Ok (distance, direction) ->
      let tanHalfAngle = Angle.tan (0.5 * theta)
       in if tanHalfAngle == Qty.zero
            then Curve2d.Line p1 p2
            else
              let offset = 0.5 * distance / tanHalfAngle
                  computedCenterPoint = Point2d.midpoint p1 p2 + offset * Direction2d.perpendicularTo direction
                  computedStartAngle = Point2d.angleFrom computedCenterPoint p1
               in Curve2d.Arc
                    computedCenterPoint
                    (Point2d.distanceFrom computedCenterPoint p1)
                    computedStartAngle
                    (computedStartAngle + theta)
    Error Vector2d.IsZero ->
      Curve2d.Line p1 p2
