{-# LANGUAGE NoFieldSelectors #-}

module Line2d
  ( from
  , directed
  , startPoint
  , endPoint
  , length
  , direction
  , build
  , StartPoint
  , EndPoint
  , Length
  , Direction
  )
where

import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Internal qualified
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
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

newtype StartPoint coordinateSystem = StartPoint (Point2d coordinateSystem)

newtype EndPoint coordinateSystem = EndPoint (Point2d coordinateSystem)

newtype Direction space = Direction (Direction2d space)

newtype Length units = Length (Qty units)

startPoint :: Point2d (space @ units) -> StartPoint (space @ units)
startPoint = StartPoint

endPoint :: Point2d (space @ units) -> EndPoint (space @ units)
endPoint = EndPoint

direction :: Direction2d space -> Direction space
direction = Direction

length :: Qty units -> Length units
length = Length

class Arguments arguments space units | arguments -> space, arguments -> units where
  build :: Tolerance units => arguments -> Result Curve2d.DegenerateCurve (Curve2d (space @ units))

instance
  (space ~ space', units ~ units') =>
  Arguments
    ( StartPoint (space @ units)
    , EndPoint (space' @ units')
    )
    space
    units
  where
  build (StartPoint givenStartPoint, EndPoint givenEndPoint) =
    from givenStartPoint givenEndPoint

instance
  (space ~ space', units ~ units') =>
  Arguments
    ( StartPoint (space @ units)
    , Direction space'
    , Length units'
    )
    space
    units
  where
  build (StartPoint givenStartPoint, Direction givenDirection, Length givenLength) =
    Ok (directed givenStartPoint givenDirection givenLength)
