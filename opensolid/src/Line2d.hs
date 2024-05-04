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
import Curve2d.Internal qualified
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)

from :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
from = Curve2d.Internal.Line

directed :: Point2d (space @ units) -> Direction2d space -> Qty units -> Curve2d (space @ units)
directed givenStartPoint givenDirection givenLength =
  from givenStartPoint (givenStartPoint + givenDirection * givenLength)

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

class Build arguments space units | arguments -> space, arguments -> units where
  build :: Tolerance units => arguments -> Curve2d (space @ units)

instance
  (space ~ space', units ~ units') =>
  Build
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
  Build
    ( StartPoint (space @ units)
    , Direction space'
    , Length units'
    )
    space
    units
  where
  build (StartPoint givenStartPoint, Direction givenDirection, Length givenLength) =
    directed givenStartPoint givenDirection givenLength
