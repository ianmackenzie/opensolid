{-# LANGUAGE NoFieldSelectors #-}

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
import Curve2d qualified
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
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
from givenStartPoint givenEndPoint givenSweptAngle =
  case Vector2d.magnitudeAndDirection (givenEndPoint - givenStartPoint) of
    Error Vector2d.IsZero -> Error Curve2d.DegenerateCurve
    Ok (distanceBetweenPoints, directionBetweenPoints)
      | linearDeviation ~= Qty.zero ->
          Ok $
            Curve2d.Internal.Line
              { startPoint = givenStartPoint
              , endPoint = givenEndPoint
              , direction = directionBetweenPoints
              , length = Point2d.distanceFrom givenStartPoint givenEndPoint
              }
      | otherwise ->
          Ok $
            Curve2d.Internal.Arc
              { centerPoint = computedCenterPoint
              , radius = Point2d.distanceFrom computedCenterPoint givenStartPoint
              , startAngle = computedStartAngle
              , endAngle = computedStartAngle + givenSweptAngle
              }
     where
      tanHalfAngle = Angle.tan (0.5 * givenSweptAngle)
      halfDistance = 0.5 * distanceBetweenPoints
      linearDeviation = halfDistance * tanHalfAngle
      offset = (halfDistance / tanHalfAngle) * Direction2d.rotateLeft directionBetweenPoints
      computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offset
      computedStartAngle = Point2d.angleFrom computedCenterPoint givenStartPoint

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

class Arguments arguments space units | arguments -> space, arguments -> units where
  build :: Tolerance units => arguments -> Result BuildError (Curve2d (space @ units))

data BuildError
  = DegenerateArc
  | EndpointsAreTooFarApart
  | NegativeRadius
  deriving (Eq, Show, Error)

instance
  (space ~ space', units ~ units') =>
  Arguments (StartPoint (space @ units), EndPoint (space' @ units'), SweptAngle) space units
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint, SweptAngle givenSweptAngle) =
    from givenStartPoint givenEndPoint givenSweptAngle ?? Error DegenerateArc

instance
  units ~ units' =>
  Arguments
    ( CenterPoint (space @ units)
    , Radius units'
    , StartAngle
    , EndAngle
    )
    space
    units
  where
  build (CenterPoint givenCenterPoint, Radius givenRadius, StartAngle givenStartAngle, EndAngle givenEndAngle)
    | givenRadius < Qty.zero = Error NegativeRadius
    | givenRadius ~= Qty.zero = Error DegenerateArc
    | givenRadius * Angle.inRadians (givenEndAngle - givenStartAngle) ~= Qty.zero = Error DegenerateArc
    | otherwise = Ok (Curve2d.Internal.Arc givenCenterPoint givenRadius givenStartAngle givenEndAngle)

instance
  units ~ units' =>
  Arguments
    ( CenterPoint (space @ units)
    , StartAngle
    , EndAngle
    , Radius units'
    )
    space
    units
  where
  build (givenCenterPoint, givenStartAngle, givenEndAngle, givenRadius) =
    build (givenCenterPoint, givenRadius, givenStartAngle, givenEndAngle)

instance
  units ~ units' =>
  Arguments
    ( CenterPoint (space @ units)
    , Radius units'
    , StartAngle
    , SweptAngle
    )
    space
    units
  where
  build (givenCenterPoint, givenRadius, StartAngle givenStartAngle, SweptAngle givenSweptAngle) =
    build
      ( givenCenterPoint
      , givenRadius
      , StartAngle givenStartAngle
      , EndAngle (givenStartAngle + givenSweptAngle)
      )

instance
  units ~ units' =>
  Arguments
    ( CenterPoint (space @ units)
    , StartAngle
    , SweptAngle
    , Radius units'
    )
    space
    units
  where
  build (givenCenterPoint, givenStartAngle, givenSweptAngle, givenRadius) =
    build (givenCenterPoint, givenRadius, givenStartAngle, givenSweptAngle)

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments (CenterPoint (space @ units), StartPoint (space' @ units'), SweptAngle) space units
  where
  build (CenterPoint givenCenterPoint, StartPoint givenStartPoint, givenSweptAngle) =
    build
      ( centerPoint givenCenterPoint
      , radius (Point2d.distanceFrom givenCenterPoint givenStartPoint)
      , startAngle (Point2d.angleFrom givenCenterPoint givenStartPoint)
      , givenSweptAngle
      )

instance
  ( space ~ space'
  , units ~ units'
  , units ~ units''
  ) =>
  Arguments
    ( StartPoint (space @ units)
    , EndPoint (space' @ units')
    , Radius units''
    , Direction
    , Size
    )
    space
    units
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint, Radius givenRadius, givenDirection, givenSize) = Result.do
    chordDirection <- Direction2d.from givenStartPoint givenEndPoint ?? Error DegenerateArc
    let squaredRadius_ = Qty.squared_ givenRadius
    let squaredHalfLength_ = Qty.squared_ (0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint)
    let squaredOffsetMagnitude_ = squaredRadius_ - squaredHalfLength_
    Result.check (squaredOffsetMagnitude_ >= Qty.zero) ?? Error Arc2d.EndpointsAreTooFarApart
    let offsetMagnitude = Qty.sqrt_ squaredOffsetMagnitude_
    let offsetDirection = Direction2d.rotateLeft chordDirection
    let offsetDistance =
          case (givenDirection, givenSize) of
            (Counterclockwise, Small) -> offsetMagnitude
            (Clockwise, Small) -> -offsetMagnitude
            (Clockwise, Large) -> offsetMagnitude
            (Counterclockwise, Large) -> -offsetMagnitude
    let computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offsetDirection * offsetDistance
    let halfLength = Qty.sqrt_ squaredHalfLength_
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
