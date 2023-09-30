module Arc2d
  ( with
  , from
  , Constraint (..)
  , BuildError
  , Direction (Clockwise, Counterclockwise)
  , Size (Large, Small)
  )
where

import Angle qualified
import CoordinateSystem (Units)
import Curve2d (Curve2d)
import Curve2d.Internal qualified
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
  | EndpointsAreTooFarApart
  | NegativeRadius
  | DegenerateArc
  deriving (Eq, Show, ErrorMessage)

with :: (Tolerance units) => List (Constraint (space @ units)) -> Result BuildError (Curve2d (space @ units))
with constraints = case List.sort constraints of
  [CenterPoint p0, Radius r, StartAngle theta1, EndAngle theta2] ->
    fromCenterPointRadiusStartAngleEndAngle p0 r theta1 theta2
  [CenterPoint p0, Radius r, StartAngle theta1, SweptAngle theta] ->
    fromCenterPointRadiusStartAngleEndAngle p0 r theta1 (theta1 + theta)
  [CenterPoint p0, StartPoint p1, SweptAngle theta] ->
    fromCenterPointStartPointSweptAngle p0 p1 theta
  [StartPoint p1, EndPoint p2, Radius r, Direction d, Size s] ->
    fromStartPointEndPointRadiusDirectionSize p1 p2 r d s
  [StartPoint p1, EndPoint p2, SweptAngle theta] ->
    from p1 p2 theta
  _ -> Error UnsupportedConstraints

fromCenterPointRadiusStartAngleEndAngle ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Qty units ->
  Angle ->
  Angle ->
  Result BuildError (Curve2d (space @ units))
fromCenterPointRadiusStartAngleEndAngle centerPoint radius startAngle endAngle
  | radius < Qty.zero = Error NegativeRadius
  | radius ~= Qty.zero = Error DegenerateArc
  | radius * Angle.inRadians (endAngle - startAngle) ~= Qty.zero = Error DegenerateArc
  | otherwise = Ok (Curve2d.Internal.Arc centerPoint radius startAngle endAngle)

fromCenterPointStartPointSweptAngle ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Angle ->
  Result BuildError (Curve2d (space @ units))
fromCenterPointStartPointSweptAngle centerPoint startPoint sweptAngle =
  let computedStartAngle = Point2d.angleFrom centerPoint startPoint
   in fromCenterPointRadiusStartAngleEndAngle
        centerPoint
        (Point2d.distanceFrom centerPoint startPoint)
        computedStartAngle
        (computedStartAngle + sweptAngle)

fromStartPointEndPointRadiusDirectionSize ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Qty units ->
  Direction ->
  Size ->
  Result BuildError (Curve2d (space @ units))
fromStartPointEndPointRadiusDirectionSize startPoint endPoint radius direction size = do
  chordDirection <-
    Direction2d.from startPoint endPoint
      |> Result.mapError (\Direction2d.PointsAreCoincident -> DegenerateArc)
  let squaredRadius = Qty.squared (Units.generalize radius)
  let squaredHalfLength =
        Qty.squared (Units.generalize (0.5 * Point2d.distanceFrom startPoint endPoint))
  squaredOffsetMagnitude <-
    Qty.nonNegative (squaredRadius - squaredHalfLength)
      |> Result.mapError (\Qty.IsNegative -> Arc2d.EndpointsAreTooFarApart)
  let offsetMagnitude = Units.specialize (Qty.sqrt squaredOffsetMagnitude)
  let offsetDirection = Direction2d.rotateLeft chordDirection
  let offsetDistance =
        case (direction, size) of
          (Counterclockwise, Small) -> offsetMagnitude
          (Clockwise, Small) -> -offsetMagnitude
          (Clockwise, Large) -> offsetMagnitude
          (Counterclockwise, Large) -> -offsetMagnitude
  let computedCenterPoint = Point2d.midpoint startPoint endPoint + offsetDirection * offsetDistance
  let halfLength = Units.specialize (Qty.sqrt squaredHalfLength)
  let shortAngle = 2.0 * Angle.asin (halfLength / radius)
  let computedSweptAngle =
        case (direction, size) of
          (Counterclockwise, Small) -> shortAngle
          (Clockwise, Small) -> -shortAngle
          (Clockwise, Large) -> shortAngle - Angle.fullTurn
          (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
  fromCenterPointStartPointSweptAngle computedCenterPoint startPoint computedSweptAngle

from ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Angle ->
  Result BuildError (Curve2d (space @ units))
from startPoint endPoint sweptAngle =
  case Vector2d.magnitudeAndDirection (endPoint - startPoint) of
    Error Vector2d.IsZero -> Error DegenerateArc
    Ok (distance, direction) ->
      -- startPoint and endPoint are distinct, distance is greater than zero
      let tanHalfAngle = Angle.tan (0.5 * sweptAngle)
          halfDistance = 0.5 * distance
          linearDeviation = halfDistance * tanHalfAngle
       in if linearDeviation ~= Qty.zero
            then Ok (Curve2d.Internal.Line startPoint endPoint direction)
            else
              let offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeft direction
                  computedCenterPoint = Point2d.midpoint startPoint endPoint + offset
                  computedStartAngle = Point2d.angleFrom computedCenterPoint startPoint
               in Ok $
                    Curve2d.Internal.Arc
                      computedCenterPoint
                      (Point2d.distanceFrom computedCenterPoint startPoint)
                      computedStartAngle
                      (computedStartAngle + sweptAngle)
