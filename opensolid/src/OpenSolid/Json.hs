module OpenSolid.Json
  ( angle
  , length
  , direction2d
  , vector2d
  , point2d
  )
where

import Angle qualified
import Direction2d (Direction2d)
import Direction2d qualified
import Json qualified
import Json.Format qualified
import Length (Length)
import Length qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Tolerance qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified

angle :: Json.Format Angle
angle =
  Json.Format.title "Angle" $
    Json.Format.description "Units: radians" $
      Json.Format.convert Angle.radians Angle.inRadians Json.Format.float

length :: Json.Format Length
length =
  Json.Format.title "Length" $
    Json.Format.description "Units: meters" $
      Json.Format.convert Length.meters Length.inMeters Json.Format.float

direction2d :: Json.Format (Direction2d space)
direction2d =
  Json.Format.title "Direction2d" $
    Json.Format.description "A direction (unit vector) in 2D space, given by its X and Y components" $
      Json.Format.examples [Direction2d.x] $
        Json.Format.validate (Tolerance.exactly Vector2d.direction) Vector2d.unit $
          Json.Format.object Vector2d.xy do
            Json.Format.requiredField "x" Vector2d.xComponent Json.Format.float
            Json.Format.requiredField "y" Vector2d.yComponent Json.Format.float

vector2d :: Json.Format (Vector2d (space @ Meters))
vector2d =
  Json.Format.title "Vector2d" $
    Json.Format.description "A displacement vector in 2D space, given by its X and Y components" $
      Json.Format.examples [Vector2d.zero] $
        Json.Format.object Vector2d.xy do
          Json.Format.requiredField "x" Vector2d.xComponent length
          Json.Format.requiredField "y" Vector2d.yComponent length

point2d :: Json.Format (Point2d (space @ Meters))
point2d =
  Json.Format.title "Point2d" $
    Json.Format.description "A position in 2D space, given by its X and Y coordinates" $
      Json.Format.examples [Point2d.origin] $
        Json.Format.object Point2d.xy do
          Json.Format.requiredField "x" Point2d.xCoordinate length
          Json.Format.requiredField "y" Point2d.yCoordinate length
