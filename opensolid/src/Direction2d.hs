module Direction2d
  ( Direction2d (Direction2d)
  , xComponent
  , yComponent
  , unsafe
  , unwrap
  , x
  , positiveX
  , negativeX
  , y
  , positiveY
  , negativeY
  , PointsAreCoincident (PointsAreCoincident)
  , from
  , fromAngle
  , toAngle
  , degrees
  , radians
  , angleFrom
  , perpendicularTo
  , rotateLeft
  , rotateRight
  , placeIn
  , relativeTo
  )
where

import Angle qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Result qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role Direction2d nominal

newtype Direction2d (space :: Type) = Direction2d_ (Vector2d (space @ Unitless))
  deriving (Eq, Show)

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Vector2d (space @ Unitless) -> Direction2d space
pattern Direction2d v <- Direction2d_ v

instance
  space ~ space' =>
  ApproximateEquality (Direction2d space) (Direction2d space') Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Angle.zero

instance Negation (Direction2d space) where
  negate (Direction2d vector) = unsafe -vector

instance Multiplication Sign (Direction2d space) (Direction2d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication (Direction2d space) Sign (Direction2d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units)) where
  scale * Direction2d vector = scale * vector

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units)) where
  Direction2d vector * scale = vector * scale

instance space ~ space' => DotProduct (Direction2d space) (Direction2d space') Float where
  Direction2d v1 <> Direction2d v2 = v1 <> v2

instance space ~ space' => CrossProduct (Direction2d space) (Direction2d space') Float where
  Direction2d v1 >< Direction2d v2 = v1 >< v2

xComponent :: Direction2d space -> Float
xComponent (Direction2d_ vector) = Vector2d.xComponent vector

yComponent :: Direction2d space -> Float
yComponent (Direction2d_ vector) = Vector2d.yComponent vector

{-# INLINE unsafe #-}
unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unsafe = Direction2d_

{-# INLINE unwrap #-}
unwrap :: Direction2d space -> Vector2d (space @ Unitless)
unwrap (Direction2d_ vector) = vector

positiveX :: Direction2d space
positiveX = unsafe (Vector2d 1.0 0.0)

negativeX :: Direction2d space
negativeX = -positiveX

positiveY :: Direction2d space
positiveY = unsafe (Vector2d 0.0 1.0)

negativeY :: Direction2d space
negativeY = -positiveY

x :: Direction2d space
x = positiveX

y :: Direction2d space
y = positiveY

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show, ErrorMessage)

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result PointsAreCoincident (Direction2d space)
from p1 p2 =
  Vector2d.direction (p2 - p1)
    |> Result.mapError (\Vector2d.IsZero -> Direction2d.PointsAreCoincident)

fromAngle :: Angle -> Direction2d space
fromAngle angle = unsafe (Vector2d.polar 1.0 angle)

toAngle :: Direction2d space -> Angle
toAngle (Direction2d (Vector2d dx dy)) = Angle.atan2 dy dx

degrees :: Float -> Direction2d space
degrees value = fromAngle (Angle.degrees value)

radians :: Float -> Direction2d space
radians value = fromAngle (Angle.radians value)

angleFrom :: Direction2d space -> Direction2d space -> Angle
angleFrom d1 d2 = Angle.atan2 (d1 >< d2) (d1 <> d2)

perpendicularTo :: Direction2d space -> Direction2d space
perpendicularTo = rotateLeft

rotateLeft :: Direction2d space -> Direction2d space
rotateLeft (Direction2d vector) = unsafe (Vector2d.rotateLeft vector)

rotateRight :: Direction2d space -> Direction2d space
rotateRight (Direction2d vector) = unsafe (Vector2d.rotateRight vector)

placeIn :: Frame2d (global @ units) (Defines local) -> Direction2d local -> Direction2d global
placeIn frame (Direction2d vector) = unsafe (Vector2d.placeIn frame vector)

relativeTo :: Frame2d (global @ units) (Defines local) -> Direction2d global -> Direction2d local
relativeTo frame (Direction2d vector) = unsafe (Vector2d.relativeTo frame vector)
