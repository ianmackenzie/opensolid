module Direction2d
  ( Direction2d (Direction2d, xComponent, yComponent)
  , unsafe
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
  )
where

import Angle (Angle)
import Angle qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Qty qualified
import Result qualified
import Units (Radians)
import Vector2d (Vector2d (..))
import Vector2d qualified

type role Direction2d phantom

data Direction2d (space :: Type) = Direction2d# {xComponent :: Float, yComponent :: Float}
  deriving (Eq, Show)

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Float -> Float -> Direction2d space
pattern Direction2d x y <- Direction2d# x y

instance
  space ~ space'
  => ApproximateEquality (Direction2d space) (Direction2d space) Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Qty.zero

instance Negation (Direction2d space) where
  negate (Direction2d dx dy) = unsafe (negate dx) (negate dy)

instance space ~ space' => DotProduct (Direction2d space) (Direction2d space') Float where
  Direction2d x1 y1 <> Direction2d x2 y2 = x1 * x2 + y1 * y2

instance space ~ space' => DotProduct (Vector2d (space @ units)) (Direction2d space') (Qty units) where
  Vector2d vx vy <> Direction2d dx dy = vx * dx + vy * dy

instance space ~ space' => DotProduct (Direction2d space) (Vector2d (space' @ units)) (Qty units) where
  Direction2d dx dy <> Vector2d vx vy = dx * vx + dy * vy

instance space ~ space' => CrossProduct (Direction2d space) (Direction2d space') Float where
  Direction2d x1 y1 >< Direction2d x2 y2 = x1 * y2 - y1 * x2

instance space ~ space' => CrossProduct (Vector2d (space @ units)) (Direction2d space') (Qty units) where
  Vector2d vx vy >< Direction2d dx dy = vx * dy - vy * dx

instance space ~ space' => CrossProduct (Direction2d space) (Vector2d (space' @ units)) (Qty units) where
  Direction2d dx dy >< Vector2d vx vy = dx * vy - dy * vx

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units)) where
  scale * Direction2d dx dy = Vector2d (scale * dx) (scale * dy)

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units)) where
  Direction2d dx dy * scale = Vector2d (dx * scale) (dy * scale)

unsafe :: Float -> Float -> Direction2d space
unsafe = Direction2d#

positiveX :: Direction2d space
positiveX = unsafe 1.0 0.0

negativeX :: Direction2d space
negativeX = -positiveX

positiveY :: Direction2d space
positiveY = unsafe 0.0 1.0

negativeY :: Direction2d space
negativeY = -positiveY

x :: Direction2d space
x = positiveX

y :: Direction2d space
y = positiveY

data PointsAreCoincident = PointsAreCoincident deriving (Show)

instance IsError PointsAreCoincident where
  errorMessage PointsAreCoincident = "Given points are coincident"

from :: Point2d (space @ units) -> Point2d (space @ units) -> Result PointsAreCoincident (Direction2d space)
from p1 p2 =
  Vector2d.direction (p2 - p1)
    |> Result.mapError \Vector2d.IsZero -> Direction2d.PointsAreCoincident

fromAngle :: Angle -> Direction2d space
fromAngle angle = unsafe (Angle.cos angle) (Angle.sin angle)

toAngle :: Direction2d space -> Angle
toAngle (Direction2d dx dy) = Angle.atan2 dy dx

degrees :: Float -> Direction2d space
degrees value = fromAngle (Angle.degrees value)

radians :: Float -> Direction2d space
radians value = fromAngle (Angle.radians value)

angleFrom :: Direction2d space -> Direction2d space -> Angle
angleFrom d1 d2 = Angle.atan2 (d1 >< d2) (d1 <> d2)

perpendicularTo :: Direction2d space -> Direction2d space
perpendicularTo (Direction2d dx dy) = unsafe -dy dx
