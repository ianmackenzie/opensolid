{-# LANGUAGE NoFieldSelectors #-}

module Line2d
  ( from
  , directed
  , startPoint
  , endPoint
  , length
  , direction
  , with
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

from :: (Tolerance units) => Point2d (space @ units) -> Point2d (space @ units) -> Result Curve2d.DegenerateCurve (Curve2d (space @ units))
from givenStartPoint givenEndPoint =
  case Direction2d.from givenStartPoint givenEndPoint of
    Error Direction2d.PointsAreCoincident -> Error Curve2d.DegenerateCurve
    Ok directionBetweenPoints ->
      Ok $
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

data Properties startPoint endPoint direction length = Properties
  { startPoint :: startPoint
  , endPoint :: endPoint
  , direction :: direction
  , length :: length
  }
  deriving (Show)

data UnspecifiedStartPoint = UnspecifiedStartPoint

data UnspecifiedEndPoint = UnspecifiedEndPoint

data UnspecifiedDirection = UnspecifiedDirection

data UnspecifiedLength = UnspecifiedLength

type EmptyProperties =
  Properties
    UnspecifiedStartPoint
    UnspecifiedEndPoint
    UnspecifiedDirection
    UnspecifiedLength

emptyProperties :: EmptyProperties
emptyProperties =
  Properties
    { startPoint = UnspecifiedStartPoint
    , endPoint = UnspecifiedEndPoint
    , direction = UnspecifiedDirection
    , length = UnspecifiedLength
    }

startPoint ::
  Point2d (space @ units) ->
  Properties UnspecifiedStartPoint endPoint direction length ->
  Properties (Point2d (space @ units)) endPoint direction length
startPoint p1 properties = properties {startPoint = p1}

endPoint ::
  Point2d (space @ units) ->
  Properties startPoint UnspecifiedEndPoint direction length ->
  Properties startPoint (Point2d (space @ units)) direction length
endPoint p1 properties = properties {endPoint = p1}

direction ::
  Direction2d space ->
  Properties startPoint endPoint UnspecifiedDirection length ->
  Properties startPoint endPoint (Direction2d space) length
direction d properties = properties {direction = d}

length ::
  Qty units ->
  Properties startPoint endPoint direction UnspecifiedLength ->
  Properties startPoint endPoint direction (Qty units)
length l properties = properties {length = l}

class
  Arguments arguments (constraint :: Constraint) result
    | arguments -> constraint
    , arguments -> result
  where
  with :: (constraint) => arguments -> result

instance
  ( p0 ~ EmptyProperties
  , Arguments p1 constraint result
  ) =>
  Arguments (p0 -> p1) constraint result
  where
  with f1 = with (emptyProperties |> f1)

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
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments
    ( Properties
        (Point2d (space @ units))
        (Point2d (space' @ units'))
        UnspecifiedDirection
        UnspecifiedLength
    )
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with
    ( Properties
        { startPoint = givenStartPoint
        , endPoint = givenEndPoint
        , direction = UnspecifiedDirection
        , length = UnspecifiedLength
        }
      ) = from givenStartPoint givenEndPoint

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments
    ( Properties
        (Point2d (space @ units))
        UnspecifiedEndPoint
        (Direction2d space')
        (Qty units')
    )
    ()
    (Curve2d (space @ units))
  where
  with
    ( Properties
        { startPoint = givenStartPoint
        , endPoint = UnspecifiedEndPoint
        , length = givenLength
        , direction = givenDirection
        }
      ) = directed givenStartPoint givenDirection givenLength

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Arguments
    (Properties UnspecifiedStartPoint (Point2d (space @ units)) (Direction2d space') (Qty units'))
    ()
    (Curve2d (space @ units))
  where
  with
    ( Properties
        { startPoint = UnspecifiedStartPoint
        , endPoint = givenEndPoint
        , length = givenLength
        , direction = givenDirection
        }
      ) =
      Curve2d.Internal.Line
        { startPoint = givenEndPoint - givenDirection * givenLength
        , endPoint = givenEndPoint
        , direction = givenDirection * Qty.sign givenLength
        , length = Qty.abs givenLength
        }

instance
  (TypeError (Text "Missing Line2d.length argument")) =>
  Arguments
    ( Properties
        (Point2d (space @ units))
        UnspecifiedEndPoint
        (Direction2d space')
        UnspecifiedLength
    )
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  (TypeError (Text "Missing Line2d.direction argument")) =>
  Arguments
    ( Properties
        (Point2d (space @ units))
        UnspecifiedEndPoint
        UnspecifiedDirection
        (Qty units')
    )
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  (TypeError (Text "Missing Line2d.startPoint or Line2d.endPoint argument")) =>
  Arguments
    ( Properties
        UnspecifiedStartPoint
        UnspecifiedEndPoint
        (Direction2d space)
        (Qty units)
    )
    ()
    (Curve2d (space @ units))
  where
  with = notImplemented

instance
  (TypeError (Text "Too many constraints")) =>
  Arguments
    ( Properties
        (Point2d (space @ units))
        (Point2d (space' @ units'))
        UnspecifiedDirection
        (Qty units'')
    )
    (Tolerance units)
    (Result Curve2d.DegenerateCurve (Curve2d (space @ units)))
  where
  with = notImplemented
