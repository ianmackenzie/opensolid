module Direction2d
  ( Direction2d (Direction2d)
  , unsafe
  , x
  , positiveX
  , negativeX
  , y
  , positiveY
  , negativeY
  , fromAngle
  , toAngle
  , degrees
  , radians
  , angleFrom
  , rotateLeft
  , rotateRight
  )
where

import Angle qualified
import OpenSolid
import Vector2d (Vector2d (..))

type role Direction2d nominal

type Direction2d :: Type -> Type
data Direction2d coordinates = Unsafe Float Float
  deriving (Eq, Show)

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Float -> Float -> Direction2d coordinates
pattern Direction2d x y <- Unsafe x y

instance Negation (Direction2d coordinates) where
  negate (Direction2d dx dy) = unsafe (negate dx) (negate dy)

instance DotProduct Direction2d Direction2d Float where
  Direction2d x1 y1 <> Direction2d x2 y2 = x1 * x2 + y1 * y2

instance DotProduct (Vector2d units) Direction2d (Qty units) where
  Vector2d vx vy <> Direction2d dx dy = vx * dx + vy * dy

instance DotProduct Direction2d (Vector2d units) (Qty units) where
  Direction2d dx dy <> Vector2d vx vy = dx * vx + dy * vy

instance coordinates ~ coordinates' => CrossProduct (Direction2d coordinates) (Direction2d coordinates') Float where
  Direction2d x1 y1 >< Direction2d x2 y2 = x1 * y2 - y1 * x2

instance coordinates ~ coordinates' => CrossProduct (Vector2d units coordinates) (Direction2d coordinates') (Qty units) where
  Vector2d vx vy >< Direction2d dx dy = vx * dy - vy * dx

instance coordinates ~ coordinates' => CrossProduct (Direction2d coordinates) (Vector2d units coordinates') (Qty units) where
  Direction2d dx dy >< Vector2d vx vy = dx * vy - dy * vx

instance Multiplication (Qty units) (Direction2d coordinates) (Vector2d units coordinates) where
  scale * Direction2d dx dy = Vector2d (scale * dx) (scale * dy)

instance Multiplication (Direction2d coordinates) (Qty units) (Vector2d units coordinates) where
  Direction2d dx dy * scale = Vector2d (dx * scale) (dy * scale)

unsafe :: Float -> Float -> Direction2d coordinates
unsafe = Unsafe

positiveX :: Direction2d coordinates
positiveX = unsafe 1.0 0.0

negativeX :: Direction2d coordinates
negativeX = negate positiveX

positiveY :: Direction2d coordinates
positiveY = unsafe 0.0 1.0

negativeY :: Direction2d coordinates
negativeY = negate positiveY

x :: Direction2d coordinates
x = positiveX

y :: Direction2d coordinates
y = positiveY

fromAngle :: Angle -> Direction2d coordinates
fromAngle angle = unsafe (Angle.cos angle) (Angle.sin angle)

toAngle :: Direction2d coordinates -> Angle
toAngle (Direction2d dx dy) = Angle.atan2 dy dx

degrees :: Float -> Direction2d coordinates
degrees value = fromAngle (Angle.degrees value)

radians :: Float -> Direction2d coordinates
radians value = fromAngle (Angle.radians value)

angleFrom :: Direction2d coordinates -> Direction2d coordinates -> Angle
angleFrom d1 d2 = Angle.atan2 (d1 >< d2) (d1 <> d2)

rotateLeft :: Direction2d coordinates -> Direction2d coordinates
rotateLeft (Direction2d dx dy) = unsafe -dy dx

rotateRight :: Direction2d coordinates -> Direction2d coordinates
rotateRight (Direction2d dx dy) = unsafe dy -dx
