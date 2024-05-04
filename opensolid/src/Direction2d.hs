module Direction2d
  ( Direction2d
  , vector
  , xComponent
  , yComponent
  , components
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
  , rotateLeft
  , rotateRight
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , generator
  , transformBy
  , rotateBy
  )
where

import Angle qualified
import {-# SOURCE #-} Basis2d (Basis2d)
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import {-# SOURCE #-} Point2d qualified
import Random qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role Direction2d phantom

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
  negate direction = Direction2d -(vector direction)

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
  scale .*. direction = scale .*. vector direction

instance Product (Qty units) (Direction2d space) (Vector2d (space @ units))

instance Multiplication (Direction2d space) (Qty units) where
  type Direction2d space .*. Qty units = Vector2d (space @ (Unitless :*: units))
  direction .*. scale = vector direction .*. scale

instance Product (Direction2d space) (Qty units) (Vector2d (space @ units))

instance space ~ space' => DotMultiplication (Direction2d space) (Direction2d space') where
  type Direction2d space .<>. Direction2d space' = Float
  Direction2d v1 .<>. Direction2d v2 = v1 <> v2

instance space ~ space' => DotProduct (Direction2d space) (Direction2d space') Float

instance space ~ space' => CrossMultiplication (Direction2d space) (Direction2d space') where
  type Direction2d space .><. Direction2d space' = Float
  Direction2d v1 .><. Direction2d v2 = v1 >< v2

instance space ~ space' => CrossProduct (Direction2d space) (Direction2d space') Float

{-# INLINE vector #-}
vector :: Direction2d space -> Vector2d (space @ Unitless)
vector (Direction2d v) = v

xComponent :: Direction2d space -> Float
xComponent direction = Vector2d.xComponent (vector direction)

yComponent :: Direction2d space -> Float
yComponent direction = Vector2d.yComponent (vector direction)

{-# INLINE components #-}
components :: Direction2d space -> (Float, Float)
components direction = Vector2d.components (vector direction)

{-# INLINE unsafe #-}
unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unsafe = Direction2d

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
rotateLeft direction = Direction2d (Vector2d.rotateLeft (vector direction))

rotateRight :: Direction2d space -> Direction2d space
rotateRight direction = Direction2d (Vector2d.rotateRight (vector direction))

placeIn :: Frame2d (global @ originUnits) (Defines local) -> Direction2d local -> Direction2d global
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo :: Frame2d (global @ originUnits) (Defines local) -> Direction2d global -> Direction2d local
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis :: Basis2d global (Defines local) -> Direction2d local -> Direction2d global
placeInBasis basis direction = unsafe (Vector2d.placeInBasis basis (vector direction))

relativeToBasis :: Basis2d global (Defines local) -> Direction2d global -> Direction2d local
relativeToBasis basis direction = Direction2d (Vector2d.relativeToBasis basis (vector direction))

generator :: Random.Generator (Direction2d space)
generator = Random.map fromAngle (Random.qty Angle.zero Angle.twoPi)

transformBy :: Transform2d.IsRigid a => Transform2d a (space @ units) -> Direction2d space -> Direction2d space
transformBy transform direction = Direction2d (Vector2d.transformBy transform (vector direction))

rotateBy :: Angle -> Direction2d space -> Direction2d space
rotateBy theta = transformBy (Transform2d.rotateAround Point2d.origin theta)
