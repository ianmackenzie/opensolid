{-# LANGUAGE NoFieldSelectors #-}

module Arc2d
  ( swept
  , with
  , centerPoint
  , startPoint
  , endPoint
  , radius
  , startAngle
  , endAngle
  , sweptAngle
  , direction
  , size
  , BuildError (..)
  , CenterPoint
  , StartPoint
  , EndPoint
  , Radius
  , StartAngle
  , EndAngle
  , SweptAngle
  , Direction (Clockwise, Counterclockwise)
  , Size (Large, Small)
  )
where

import Angle qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Internal qualified
import Data.Kind (Constraint)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Result qualified
import Units qualified
import Vector2d qualified

swept ::
  (Tolerance units) =>
  Angle ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
swept givenSweptAngle givenStartPoint givenEndPoint =
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

data
  Properties
    centerPoint
    startPoint
    endPoint
    radius
    startAngle
    endAngle
    sweptAngle
    direction
    size = Properties
  { centerPoint :: centerPoint
  , startPoint :: startPoint
  , endPoint :: endPoint
  , radius :: radius
  , startAngle :: startAngle
  , endAngle :: endAngle
  , sweptAngle :: sweptAngle
  , direction :: direction
  , size :: size
  }

newtype CenterPoint coordinateSystem = CenterPoint (Point2d coordinateSystem)

newtype StartPoint coordinateSystem = StartPoint (Point2d coordinateSystem)

newtype EndPoint coordinateSystem = EndPoint (Point2d coordinateSystem)

newtype Radius units = Radius (Qty units)

newtype StartAngle = StartAngle Angle

newtype EndAngle = EndAngle Angle

newtype SweptAngle = SweptAngle Angle

data Direction = Clockwise | Counterclockwise deriving (Eq, Ord)

data Size = Large | Small deriving (Eq, Ord)

type EmptyProperties = Properties () () () () () () () () ()

emptyProperties :: EmptyProperties
emptyProperties = Properties () () () () () () () () ()

centerPoint ::
  Point2d (space @ units) ->
  Properties () startPoint endPoint radius startAngle endAngle sweptAngle direction size ->
  Properties (CenterPoint (space @ units)) startPoint endPoint radius startAngle endAngle sweptAngle direction size
centerPoint givenCenterPoint properties = properties {centerPoint = CenterPoint givenCenterPoint}

startPoint ::
  Point2d (space @ units) ->
  Properties centerPoint () endPoint radius startAngle endAngle sweptAngle direction size ->
  Properties centerPoint (StartPoint (space @ units)) endPoint radius startAngle endAngle sweptAngle direction size
startPoint givenStartPoint properties = properties {startPoint = StartPoint givenStartPoint}

endPoint ::
  Point2d (space @ units) ->
  Properties centerPoint startPoint () radius startAngle endAngle sweptAngle direction size ->
  Properties centerPoint startPoint (EndPoint (space @ units)) radius startAngle endAngle sweptAngle direction size
endPoint givenEndPoint properties = properties {endPoint = EndPoint givenEndPoint}

radius ::
  Qty units ->
  Properties centerPoint startPoint endPoint () startAngle endAngle sweptAngle direction size ->
  Properties centerPoint startPoint endPoint (Radius units) startAngle endAngle sweptAngle direction size
radius givenRadius properties = properties {radius = Radius givenRadius}

startAngle ::
  Angle ->
  Properties centerPoint startPoint endPoint radius () endAngle sweptAngle direction size ->
  Properties centerPoint startPoint endPoint radius StartAngle endAngle sweptAngle direction size
startAngle givenStartAngle properties = properties {startAngle = StartAngle givenStartAngle}

endAngle ::
  Angle ->
  Properties centerPoint startPoint endPoint radius startAngle () sweptAngle direction size ->
  Properties centerPoint startPoint endPoint radius startAngle EndAngle sweptAngle direction size
endAngle givenEndAngle properties = properties {endAngle = EndAngle givenEndAngle}

sweptAngle ::
  Angle ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle () direction size ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle SweptAngle direction size
sweptAngle givenSweptAngle properties = properties {sweptAngle = SweptAngle givenSweptAngle}

direction ::
  Direction ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle sweptAngle () size ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle sweptAngle Direction size
direction givenDirection properties = properties {direction = givenDirection}

size ::
  Size ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle sweptAngle direction () ->
  Properties centerPoint startPoint endPoint radius startAngle endAngle sweptAngle direction Size
size givenSize properties = properties {size = givenSize}

class
  Arguments arguments (constraint :: Constraint) result
    | arguments -> constraint
    , arguments -> result
  where
  with :: (constraint) => arguments -> result

instance
  ( p0 ~ EmptyProperties
  , p1 ~ p1'
  , Arguments p2 constraint result
  ) =>
  Arguments (p0 -> p1, p1' -> p2) constraint result
  where
  with (f1, f2) = with (emptyProperties |> f1 |> f2)

instance
  ( p0 ~ EmptyProperties
  , p1 ~ p1'
  , p2 ~ p2'
  , Arguments p3 constraint result
  ) =>
  Arguments (p0 -> p1, p1' -> p2, p2' -> p3) constraint result
  where
  with (f1, f2, f3) = with (emptyProperties |> f1 |> f2 |> f3)

instance
  ( p0 ~ EmptyProperties
  , p1 ~ p1'
  , p2 ~ p2'
  , p3 ~ p3'
  , Arguments p4 constraint result
  ) =>
  Arguments (p0 -> p1, p1' -> p2, p2' -> p3, p3' -> p4) constraint result
  where
  with (f1, f2, f3, f4) = with (emptyProperties |> f1 |> f2 |> f3 |> f4)

instance
  ( p0 ~ EmptyProperties
  , p1 ~ p1'
  , p2 ~ p2'
  , p3 ~ p3'
  , p4 ~ p4'
  , Arguments p5 constraint result
  ) =>
  Arguments (p0 -> p1, p1' -> p2, p2' -> p3, p3' -> p4, p4' -> p5) constraint result
  where
  with (f1, f2, f3, f4, f5) = with (emptyProperties |> f1 |> f2 |> f3 |> f4 |> f5)

data BuildError
  = DegenerateArc
  | EndpointsAreTooFarApart
  | NegativeRadius
  deriving (Eq, Show, ErrorMessage)

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments
    (Properties () (StartPoint (space @ units)) (EndPoint (space' @ units')) () () () SweptAngle () ())
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with properties = swept givenSweptAngle givenStartPoint givenEndPoint
   where
    Properties
      { startPoint = StartPoint givenStartPoint
      , endPoint = EndPoint givenEndPoint
      , sweptAngle = SweptAngle givenSweptAngle
      } = properties

instance
  (units ~ units') =>
  Arguments
    (Properties (CenterPoint (space @ units)) () () (Radius units') StartAngle EndAngle () () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with properties
    | givenRadius < Qty.zero = Error NegativeRadius
    | givenRadius ~= Qty.zero = Error DegenerateArc
    | givenRadius * Angle.inRadians (givenEndAngle - givenStartAngle) ~= Qty.zero = Error DegenerateArc
    | otherwise = Ok (Curve2d.Internal.Arc givenCenterPoint givenRadius givenStartAngle givenEndAngle)
   where
    Properties
      { centerPoint = CenterPoint givenCenterPoint
      , radius = Radius givenRadius
      , startAngle = StartAngle givenStartAngle
      , endAngle = EndAngle givenEndAngle
      } = properties

instance
  (units ~ units') =>
  Arguments
    (Properties (CenterPoint (space @ units)) () () (Radius units') StartAngle () SweptAngle () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with properties =
    with
      ( centerPoint givenCenterPoint
      , radius givenRadius
      , startAngle givenStartAngle
      , endAngle (givenStartAngle + givenSweptAngle)
      )
   where
    Properties
      { centerPoint = CenterPoint givenCenterPoint
      , radius = Radius givenRadius
      , startAngle = StartAngle givenStartAngle
      , sweptAngle = SweptAngle givenSweptAngle
      } = properties

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments
    (Properties (CenterPoint (space @ units)) (StartPoint (space' @ units')) () () () () SweptAngle () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with properties =
    let computedStartAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
     in with
          ( centerPoint givenCenterPoint
          , radius (Point2d.distanceFrom givenCenterPoint givenStartPoint)
          , startAngle computedStartAngle
          , endAngle (computedStartAngle + givenSweptAngle)
          )
   where
    Properties
      { centerPoint = CenterPoint givenCenterPoint
      , startPoint = StartPoint givenStartPoint
      , sweptAngle = SweptAngle givenSweptAngle
      } = properties

instance
  ( space ~ space'
  , units ~ units'
  , units ~ units''
  ) =>
  Arguments
    (Properties () (StartPoint (space @ units)) (EndPoint (space' @ units')) (Radius units'') () () () Direction Size)
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with properties = do
    chordDirection <-
      Direction2d.from givenStartPoint givenEndPoint
        |> Result.mapError (\Direction2d.PointsAreCoincident -> DegenerateArc)
    let squaredRadius = Qty.squared (Units.generalize givenRadius)
    let squaredHalfLength =
          Qty.squared (Units.generalize (0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint))
    squaredOffsetMagnitude <-
      Qty.nonNegative (squaredRadius - squaredHalfLength)
        |> Result.mapError (\Qty.IsNegative -> Arc2d.EndpointsAreTooFarApart)
    let offsetMagnitude = Units.specialize (Qty.sqrt squaredOffsetMagnitude)
    let offsetDirection = Direction2d.rotateLeft chordDirection
    let offsetDistance =
          case (givenDirection, givenSize) of
            (Counterclockwise, Small) -> offsetMagnitude
            (Clockwise, Small) -> -offsetMagnitude
            (Clockwise, Large) -> offsetMagnitude
            (Counterclockwise, Large) -> -offsetMagnitude
    let computedCenterPoint = Point2d.midpoint givenStartPoint givenEndPoint + offsetDirection * offsetDistance
    let halfLength = Units.specialize (Qty.sqrt squaredHalfLength)
    let shortAngle = 2.0 * Angle.asin (halfLength / givenRadius)
    let computedSweptAngle =
          case (givenDirection, givenSize) of
            (Counterclockwise, Small) -> shortAngle
            (Clockwise, Small) -> -shortAngle
            (Clockwise, Large) -> shortAngle - Angle.fullTurn
            (Counterclockwise, Large) -> Angle.fullTurn - shortAngle
    with
      ( centerPoint computedCenterPoint
      , startPoint givenStartPoint
      , sweptAngle computedSweptAngle
      )
   where
    Properties
      { startPoint = StartPoint givenStartPoint
      , endPoint = EndPoint givenEndPoint
      , radius = Radius givenRadius
      , direction = givenDirection
      , size = givenSize
      } = properties
