module Direction2d
  ( Direction2d
  , unitVector
  , xComponent
  , yComponent
  , components
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
  , placeInBasis
  , relativeToBasis
  , generator
  )
where

import Angle qualified
import {-# SOURCE #-} Basis2d (Basis2d)
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Random qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role Direction2d nominal

newtype Direction2d (space :: Type) = Direction2d (Vector2d (space @ Unitless))
  deriving (Eq, Show)

instance HasUnits (Direction2d space) where
  type Units (Direction2d space) = Unitless
  type Erase (Direction2d space) = Direction2d space

instance space ~ space' => Units.Coercion (Direction2d space) (Direction2d space') where
  coerce = identity

instance
  space ~ space' =>
  ApproximateEquality (Direction2d space) (Direction2d space') Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Angle.zero

instance Negation (Direction2d space) where
  negate (Direction2d vector) = unsafe -vector

instance Multiplication Sign (Direction2d space) where
  type Sign .*. Direction2d space = Direction2d space
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Product Sign (Direction2d space) (Direction2d space)

instance Multiplication (Direction2d space) Sign where
  type Direction2d space .*. Sign = Direction2d space
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance Product (Direction2d space) Sign (Direction2d space)

instance Multiplication (Qty units) (Direction2d space) where
  type Qty units .*. Direction2d space = Vector2d (space @ (units :*: Unitless))
  scale .*. Direction2d vector = scale .*. vector

instance Product (Qty units) (Direction2d space) (Vector2d (space @ units))

instance Multiplication (Direction2d space) (Qty units) where
  type Direction2d space .*. Qty units = Vector2d (space @ (Unitless :*: units))
  Direction2d vector .*. scale = vector .*. scale

instance Product (Direction2d space) (Qty units) (Vector2d (space @ units))

instance space ~ space' => DotMultiplication (Direction2d space) (Direction2d space') where
  type Direction2d space .<>. Direction2d space' = Float
  Direction2d v1 .<>. Direction2d v2 = v1 <> v2

instance space ~ space' => DotProduct (Direction2d space) (Direction2d space') Float

instance space ~ space' => CrossMultiplication (Direction2d space) (Direction2d space') where
  type Direction2d space .><. Direction2d space' = Float
  Direction2d v1 .><. Direction2d v2 = v1 >< v2

instance space ~ space' => CrossProduct (Direction2d space) (Direction2d space') Float

{-# INLINE unitVector #-}
unitVector :: Direction2d space -> Vector2d (space @ Unitless)
unitVector (Direction2d vector) = vector

xComponent :: Direction2d space -> Float
xComponent (Direction2d vector) = Vector2d.xComponent vector

yComponent :: Direction2d space -> Float
yComponent (Direction2d vector) = Vector2d.yComponent vector

{-# INLINE components #-}
components :: Direction2d space -> (Float, Float)
components (Direction2d vector) = Vector2d.components vector

{-# INLINE unsafe #-}
unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unsafe = Direction2d

{-# INLINE unwrap #-}
unwrap :: Direction2d space -> Vector2d (space @ Unitless)
unwrap (Direction2d vector) = vector

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

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show, Error)

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result PointsAreCoincident (Direction2d space)
from p1 p2 = Vector2d.direction (p2 - p1) ?? Error PointsAreCoincident

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
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo :: Frame2d (global @ units) (Defines local) -> Direction2d global -> Direction2d local
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis :: Basis2d global (Defines local) -> Direction2d local -> Direction2d global
placeInBasis basis (Direction2d vector) = unsafe (Vector2d.placeInBasis basis vector)

relativeToBasis :: Basis2d global (Defines local) -> Direction2d global -> Direction2d local
relativeToBasis basis (Direction2d vector) = unsafe (Vector2d.relativeToBasis basis vector)

generator :: Random.Generator (Direction2d space)
generator = Random.map fromAngle (Random.qty Angle.zero Angle.twoPi)
