{-# LANGUAGE NoFieldSelectors #-}

module Line2d
  ( from
  , directed
  , startPoint
  , endPoint
  , length
  , direction
  , with
  , StartPoint
  , EndPoint
  , Direction
  )
where

import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Internal qualified
import Data.Kind (Constraint)
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid hiding ((>>))
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Type.Errors (ErrorMessage (Text), TypeError)

from :: Tolerance units => Point2d (space @ units) -> Point2d (space @ units) -> Result Curve2d.DegenerateCurve (Curve2d (space @ units))
from givenStartPoint givenEndPoint =
  case Direction2d.from givenStartPoint givenEndPoint of
    Error Direction2d.PointsAreCoincident -> Error Curve2d.DegenerateCurve
    Ok directionBetweenPoints ->
      Ok <|
        Curve2d.Internal.Line
          { startPoint = givenStartPoint
          , endPoint = givenEndPoint
          , direction = directionBetweenPoints
          , length = Point2d.distanceFrom givenStartPoint givenEndPoint
          }

directed :: Point2d (space @ units) -> Direction2d space -> Qty units -> Curve2d (space @ units)
directed givenStartPoint givenDirection givenLength =
  Curve2d.Internal.Line
    { startPoint = givenStartPoint
    , endPoint = givenStartPoint + givenDirection * givenLength
    , direction = givenDirection * Qty.sign givenLength
    , length = Qty.abs givenLength
    }

data Arguments startPoint endPoint direction length = Arguments
  { startPoint :: startPoint
  , endPoint :: endPoint
  , direction :: direction
  , length :: length
  }
  deriving (Show)

newtype StartPoint coordinateSystem = StartPoint (Point2d coordinateSystem)

newtype EndPoint coordinateSystem = EndPoint (Point2d coordinateSystem)

newtype Direction space = Direction (Direction2d space)

newtype Length units = Length (Qty units)

type NoArguments = Arguments () () () ()

noArguments :: NoArguments
noArguments = Arguments () () () ()

startPoint ::
  Point2d (space @ units) ->
  Arguments () endPoint direction length ->
  Arguments (StartPoint (space @ units)) endPoint direction length
startPoint givenStartPoint arguments = arguments {startPoint = StartPoint givenStartPoint}

endPoint ::
  Point2d (space @ units) ->
  Arguments startPoint () direction length ->
  Arguments startPoint (EndPoint (space @ units)) direction length
endPoint givenEndPoint arguments = arguments {endPoint = EndPoint givenEndPoint}

direction ::
  Direction2d space ->
  Arguments startPoint endPoint () length ->
  Arguments startPoint endPoint (Direction space) length
direction givenDirection arguments = arguments {direction = Direction givenDirection}

length ::
  Qty units ->
  Arguments startPoint endPoint direction () ->
  Arguments startPoint endPoint direction (Length units)
length givenLength arguments = arguments {length = Length givenLength}

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
  ( space ~ space'
  , units ~ units'
  ) =>
  Build
    (Arguments (StartPoint (space @ units)) (EndPoint (space' @ units')) () ())
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with arguments = from givenStartPoint givenEndPoint
   where
    Arguments
      { startPoint = StartPoint givenStartPoint
      , endPoint = EndPoint givenEndPoint
      } = arguments

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Build
    (Arguments (StartPoint (space @ units)) () (Direction space') (Length units'))
    ()
    (Curve2d (space @ units))
  where
  with arguments = directed givenStartPoint givenDirection givenLength
   where
    Arguments
      { startPoint = StartPoint givenStartPoint
      , length = Length givenLength
      , direction = Direction givenDirection
      } = arguments

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Build
    (Arguments () (EndPoint (space @ units)) (Direction space') (Length units'))
    ()
    (Curve2d (space @ units))
  where
  with arguments =
    Curve2d.Internal.Line
      { startPoint = givenEndPoint - givenDirection * givenLength
      , endPoint = givenEndPoint
      , direction = givenDirection * Qty.sign givenLength
      , length = Qty.abs givenLength
      }
   where
    Arguments
      { endPoint = EndPoint givenEndPoint
      , length = Length givenLength
      , direction = Direction givenDirection
      } = arguments

instance
  TypeError (Text "Missing Line2d.length argument") =>
  Build
    (Arguments (StartPoint (space @ units)) () (Direction space') ())
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  TypeError (Text "Missing Line2d.direction argument") =>
  Build
    (Arguments (StartPoint (space @ units)) () () (Length units'))
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  TypeError (Text "Missing Line2d.startPoint or Line2d.endPoint argument") =>
  Build
    (Arguments () () (Direction space) (Length units))
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  TypeError (Text "Too many constraints") =>
  Build
    (Arguments (StartPoint (space @ units)) (EndPoint (space' @ units')) () (Length units''))
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with = notImplemented
