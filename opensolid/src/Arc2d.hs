module Arc2d
  ( from
  , build
  , centerPoint
  , startPoint
  , endPoint
  , radius
  , startAngle
  , endAngle
  , sweptAngle
  , counterclockwise
  , clockwise
  , small
  , large
  , BuildError (..)
  , CenterPoint
  , StartPoint
  , EndPoint
  , Radius
  , StartAngle
  , EndAngle
  , SweptAngle
  , Direction
  , Size
  )
where

import Angle qualified
import Curve2d (Curve2d)
import Curve2d.Internal qualified
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Result qualified
import Vector2d qualified

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Angle ->
  Curve2d (space @ units)
from givenStartPoint givenEndPoint givenSweptAngle =
  case Vector2d.magnitudeAndDirection (givenEndPoint - givenStartPoint) of
    Error Vector2d.IsZero -> Curve2d.Internal.Line givenStartPoint givenEndPoint
    Ok (distanceBetweenPoints, directionBetweenPoints) -> do
      let halfDistance = 0.5 * distanceBetweenPoints
      let tanHalfAngle = Angle.tan (0.5 * givenSweptAngle)
      let linearDeviation = halfDistance * tanHalfAngle
      if linearDeviation ~= Qty.zero
        then Curve2d.Internal.Line givenStartPoint givenEndPoint
        else do
          let offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeft directionBetweenPoints
          let computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offset
          let computedStartAngle = Point2d.angleFrom computedCenterPoint givenStartPoint
          let r = Point2d.distanceFrom computedCenterPoint givenStartPoint
          Curve2d.Internal.Arc
            computedCenterPoint
            (Vector2d.x r)
            (Vector2d.y r)
            computedStartAngle
            (computedStartAngle + givenSweptAngle)

newtype CenterPoint coordinateSystem = CenterPoint (Point2d coordinateSystem)

newtype StartPoint coordinateSystem = StartPoint (Point2d coordinateSystem)

newtype EndPoint coordinateSystem = EndPoint (Point2d coordinateSystem)

newtype Radius units = Radius (Qty units)

newtype StartAngle = StartAngle Angle

newtype EndAngle = EndAngle Angle

newtype SweptAngle = SweptAngle Angle

data Direction = Clockwise | Counterclockwise deriving (Eq, Ord)

data Size = Large | Small deriving (Eq, Ord)

centerPoint :: Point2d (space @ units) -> CenterPoint (space @ units)
centerPoint = CenterPoint

startPoint :: Point2d (space @ units) -> StartPoint (space @ units)
startPoint = StartPoint

endPoint :: Point2d (space @ units) -> EndPoint (space @ units)
endPoint = EndPoint

radius :: Qty units -> Radius units
radius = Radius

startAngle :: Angle -> StartAngle
startAngle = StartAngle

endAngle :: Angle -> EndAngle
endAngle = EndAngle

sweptAngle :: Angle -> SweptAngle
sweptAngle = SweptAngle

counterclockwise :: Direction
counterclockwise = Counterclockwise

clockwise :: Direction
clockwise = Clockwise

small :: Size
small = Small

large :: Size
large = Large

class Build arguments (coordinateSystem :: CoordinateSystem) | arguments -> coordinateSystem where
  build ::
    Tolerance (Units coordinateSystem) =>
    arguments ->
    Result BuildError (Curve2d coordinateSystem)

data BuildError
  = DegenerateArc
  | EndpointsAreTooFarApart
  | NegativeRadius
  deriving (Eq, Show, Error)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Build (StartPoint (space @ units), EndPoint (space_ @ units_), SweptAngle) (space @ units)
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint, SweptAngle givenSweptAngle) =
    Ok (from givenStartPoint givenEndPoint givenSweptAngle)

instance
  units ~ units_ =>
  Build
    ( CenterPoint (space @ units)
    , Radius units_
    , StartAngle
    , EndAngle
    )
    (space @ units)
  where
  build (CenterPoint givenCenterPoint, Radius givenRadius, StartAngle givenStartAngle, EndAngle givenEndAngle)
    | givenRadius < Qty.zero = Error NegativeRadius
    | givenRadius ~= Qty.zero = Error DegenerateArc
    | givenRadius * Angle.inRadians (givenEndAngle - givenStartAngle) ~= Qty.zero = Error DegenerateArc
    | otherwise =
        Ok $
          Curve2d.Internal.Arc
            givenCenterPoint
            (Vector2d.x givenRadius)
            (Vector2d.y givenRadius)
            givenStartAngle
            givenEndAngle

instance
  units ~ units_ =>
  Build
    ( CenterPoint (space @ units)
    , StartAngle
    , EndAngle
    , Radius units_
    )
    (space @ units)
  where
  build (givenCenterPoint, givenStartAngle, givenEndAngle, givenRadius) =
    build (givenCenterPoint, givenRadius, givenStartAngle, givenEndAngle)

instance
  units ~ units_ =>
  Build
    ( CenterPoint (space @ units)
    , Radius units_
    , StartAngle
    , SweptAngle
    )
    (space @ units)
  where
  build (givenCenterPoint, givenRadius, StartAngle givenStartAngle, SweptAngle givenSweptAngle) =
    build
      ( givenCenterPoint
      , givenRadius
      , StartAngle givenStartAngle
      , EndAngle (givenStartAngle + givenSweptAngle)
      )

instance
  units ~ units_ =>
  Build
    ( CenterPoint (space @ units)
    , StartAngle
    , SweptAngle
    , Radius units_
    )
    (space @ units)
  where
  build (givenCenterPoint, givenStartAngle, givenSweptAngle, givenRadius) =
    build (givenCenterPoint, givenRadius, givenStartAngle, givenSweptAngle)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Build (CenterPoint (space @ units), StartPoint (space_ @ units_), SweptAngle) (space @ units)
  where
  build (CenterPoint givenCenterPoint, StartPoint givenStartPoint, givenSweptAngle) =
    build
      ( centerPoint givenCenterPoint
      , radius (Point2d.distanceFrom givenCenterPoint givenStartPoint)
      , startAngle (Point2d.angleFrom givenCenterPoint givenStartPoint)
      , givenSweptAngle
      )

instance
  ( space ~ space_
  , units ~ units_
  , units ~ units__
  ) =>
  Build
    ( StartPoint (space @ units)
    , EndPoint (space_ @ units_)
    , Radius units__
    , Direction
    , Size
    )
    (space @ units)
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint, Radius givenRadius, givenDirection, givenSize) = Result.do
    chordDirection <- Direction2d.from givenStartPoint givenEndPoint ?? Error DegenerateArc
    let squaredRadius' = Qty.squared' givenRadius
    let squaredHalfLength' = Qty.squared' (0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint)
    let squaredOffsetMagnitude' = squaredRadius' - squaredHalfLength'
    Result.check (squaredOffsetMagnitude' >= Qty.zero) ?? Error Arc2d.EndpointsAreTooFarApart
    let offsetMagnitude = Qty.sqrt' squaredOffsetMagnitude'
    let offsetDirection = Direction2d.rotateLeft chordDirection
    let offsetDistance =
          case (givenDirection, givenSize) of
            (Counterclockwise, Small) -> offsetMagnitude
            (Clockwise, Small) -> -offsetMagnitude
            (Clockwise, Large) -> offsetMagnitude
            (Counterclockwise, Large) -> -offsetMagnitude
    let computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offsetDirection * offsetDistance
    let halfLength = Qty.sqrt' squaredHalfLength'
    let shortAngle = 2.0 * Angle.asin (halfLength / givenRadius)
    let computedSweptAngle =
          case (givenDirection, givenSize) of
            (Counterclockwise, Small) -> shortAngle
            (Clockwise, Small) -> -shortAngle
            (Clockwise, Large) -> shortAngle - Angle.fullTurn
            (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
    build
      ( centerPoint computedCenterPoint
      , startPoint givenStartPoint
      , sweptAngle computedSweptAngle
      )
