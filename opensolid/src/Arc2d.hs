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
import Type.Errors (TypeError)
import Type.Errors qualified
import Units qualified
import Vector2d qualified

swept ::
  Tolerance units =>
  Angle ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
swept givenSweptAngle givenStartPoint givenEndPoint =
  case Vector2d.magnitudeAndDirection (givenEndPoint - givenStartPoint) of
    Error Vector2d.IsZero -> Error Curve2d.DegenerateCurve
    Ok (distanceBetweenPoints, directionBetweenPoints)
      | linearDeviation ~= Qty.zero ->
          Ok <|
            Curve2d.Internal.Line
              { startPoint = givenStartPoint
              , endPoint = givenEndPoint
              , direction = directionBetweenPoints
              , length = Point2d.distanceFrom givenStartPoint givenEndPoint
              }
      | otherwise ->
          Ok <|
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
  Arguments
    centerPoint
    startPoint
    endPoint
    radius
    startAngle
    endAngle
    sweptAngle
    direction
    size = Arguments
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

type NoArguments = Arguments () () () () () () () () ()

noArguments :: NoArguments
noArguments = Arguments () () () () () () () () ()

centerPoint ::
  Point2d (space @ units) ->
  Arguments () startPoint endPoint radius startAngle endAngle sweptAngle direction size ->
  Arguments (CenterPoint (space @ units)) startPoint endPoint radius startAngle endAngle sweptAngle direction size
centerPoint givenCenterPoint arguments = arguments {centerPoint = CenterPoint givenCenterPoint}

startPoint ::
  Point2d (space @ units) ->
  Arguments centerPoint () endPoint radius startAngle endAngle sweptAngle direction size ->
  Arguments centerPoint (StartPoint (space @ units)) endPoint radius startAngle endAngle sweptAngle direction size
startPoint givenStartPoint arguments = arguments {startPoint = StartPoint givenStartPoint}

endPoint ::
  Point2d (space @ units) ->
  Arguments centerPoint startPoint () radius startAngle endAngle sweptAngle direction size ->
  Arguments centerPoint startPoint (EndPoint (space @ units)) radius startAngle endAngle sweptAngle direction size
endPoint givenEndPoint arguments = arguments {endPoint = EndPoint givenEndPoint}

radius ::
  Qty units ->
  Arguments centerPoint startPoint endPoint () startAngle endAngle sweptAngle direction size ->
  Arguments centerPoint startPoint endPoint (Radius units) startAngle endAngle sweptAngle direction size
radius givenRadius arguments = arguments {radius = Radius givenRadius}

startAngle ::
  Angle ->
  Arguments centerPoint startPoint endPoint radius () endAngle sweptAngle direction size ->
  Arguments centerPoint startPoint endPoint radius StartAngle endAngle sweptAngle direction size
startAngle givenStartAngle arguments = arguments {startAngle = StartAngle givenStartAngle}

endAngle ::
  Angle ->
  Arguments centerPoint startPoint endPoint radius startAngle () sweptAngle direction size ->
  Arguments centerPoint startPoint endPoint radius startAngle EndAngle sweptAngle direction size
endAngle givenEndAngle arguments = arguments {endAngle = EndAngle givenEndAngle}

sweptAngle ::
  Angle ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle () direction size ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle SweptAngle direction size
sweptAngle givenSweptAngle arguments = arguments {sweptAngle = SweptAngle givenSweptAngle}

direction ::
  Direction ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle sweptAngle () size ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle sweptAngle Direction size
direction givenDirection arguments = arguments {direction = givenDirection}

size ::
  Size ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle sweptAngle direction () ->
  Arguments centerPoint startPoint endPoint radius startAngle endAngle sweptAngle direction Size
size givenSize arguments = arguments {size = givenSize}

class
  Build arguments (constraint :: Constraint) result
    | arguments -> constraint
    , arguments -> result
  where
  with :: constraint => arguments -> result

instance
  ( a0 ~ NoArguments
  , a1 ~ a1'
  , Build a2 constraint result
  ) =>
  Build (a0 -> a1, a1' -> a2) constraint result
  where
  with (a1, a2) = with (noArguments |> a1 |> a2)

instance
  ( a0 ~ NoArguments
  , a1 ~ a1'
  , a2 ~ a2'
  , Build a3 constraint result
  ) =>
  Build (a0 -> a1, a1' -> a2, a2' -> a3) constraint result
  where
  with (a1, a2, a3) = with (noArguments |> a1 |> a2 |> a3)

instance
  ( a0 ~ NoArguments
  , a1 ~ a1'
  , a2 ~ a2'
  , a3 ~ a3'
  , Build a4 constraint result
  ) =>
  Build (a0 -> a1, a1' -> a2, a2' -> a3, a3' -> a4) constraint result
  where
  with (a1, a2, a3, a4) = with (noArguments |> a1 |> a2 |> a3 |> a4)

instance
  ( a0 ~ NoArguments
  , a1 ~ a1'
  , a2 ~ a2'
  , a3 ~ a3'
  , a4 ~ a4'
  , Build a5 constraint result
  ) =>
  Build (a0 -> a1, a1' -> a2, a2' -> a3, a3' -> a4, a4' -> a5) constraint result
  where
  with (a1, a2, a3, a4, a5) = with (noArguments |> a1 |> a2 |> a3 |> a4 |> a5)

data BuildError
  = DegenerateArc
  | EndpointsAreTooFarApart
  | NegativeRadius
  deriving (Eq, Show, Error)

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Build
    (Arguments () (StartPoint (space @ units)) (EndPoint (space' @ units')) () () () SweptAngle () ())
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with arguments = swept givenSweptAngle givenStartPoint givenEndPoint
   where
    Arguments
      { startPoint = StartPoint givenStartPoint
      , endPoint = EndPoint givenEndPoint
      , sweptAngle = SweptAngle givenSweptAngle
      } = arguments

instance
  units ~ units' =>
  Build
    (Arguments (CenterPoint (space @ units)) () () (Radius units') StartAngle EndAngle () () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with arguments
    | givenRadius < Qty.zero = Error NegativeRadius
    | givenRadius ~= Qty.zero = Error DegenerateArc
    | givenRadius * Angle.inRadians (givenEndAngle - givenStartAngle) ~= Qty.zero = Error DegenerateArc
    | otherwise = Ok (Curve2d.Internal.Arc givenCenterPoint givenRadius givenStartAngle givenEndAngle)
   where
    Arguments
      { centerPoint = CenterPoint givenCenterPoint
      , radius = Radius givenRadius
      , startAngle = StartAngle givenStartAngle
      , endAngle = EndAngle givenEndAngle
      } = arguments

instance
  units ~ units' =>
  Build
    (Arguments (CenterPoint (space @ units)) () () (Radius units') StartAngle () SweptAngle () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with arguments =
    with
      ( centerPoint givenCenterPoint
      , radius givenRadius
      , startAngle givenStartAngle
      , endAngle (givenStartAngle + givenSweptAngle)
      )
   where
    Arguments
      { centerPoint = CenterPoint givenCenterPoint
      , radius = Radius givenRadius
      , startAngle = StartAngle givenStartAngle
      , sweptAngle = SweptAngle givenSweptAngle
      } = arguments

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Build
    (Arguments (CenterPoint (space @ units)) (StartPoint (space' @ units')) () () () () SweptAngle () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with arguments =
    let computedStartAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
     in with
          ( centerPoint givenCenterPoint
          , radius (Point2d.distanceFrom givenCenterPoint givenStartPoint)
          , startAngle computedStartAngle
          , endAngle (computedStartAngle + givenSweptAngle)
          )
   where
    Arguments
      { centerPoint = CenterPoint givenCenterPoint
      , startPoint = StartPoint givenStartPoint
      , sweptAngle = SweptAngle givenSweptAngle
      } = arguments

instance
  ( space ~ space'
  , units ~ units'
  , units ~ units''
  ) =>
  Build
    (Arguments () (StartPoint (space @ units)) (EndPoint (space' @ units')) (Radius units'') () () () Direction Size)
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with arguments = do
    chordDirection <-
      Direction2d.from givenStartPoint givenEndPoint
        |> Result.mapError (\Direction2d.PointsAreCoincident -> DegenerateArc)
    let squaredRadius = Qty.squared (Units.generalize givenRadius)
    let squaredHalfLength =
          Qty.squared (Units.generalize (0.5 * Point2d.distanceFrom givenStartPoint givenEndPoint))
    let squaredOffsetMagnitude = squaredRadius - squaredHalfLength
    Result.check (squaredOffsetMagnitude >= Qty.zero) Arc2d.EndpointsAreTooFarApart
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
    Arguments
      { startPoint = StartPoint givenStartPoint
      , endPoint = EndPoint givenEndPoint
      , radius = Radius givenRadius
      , direction = givenDirection
      , size = givenSize
      } = arguments

instance
  TypeError (Type.Errors.Text "Missing Arc2d.sweptAngle argument") =>
  Build
    (Arguments () (StartPoint (space @ units)) (EndPoint (space' @ units')) () () () () () ())
    (Tolerance units)
    (Result BuildError (Curve2d (space @ units)))
  where
  with = notImplemented
