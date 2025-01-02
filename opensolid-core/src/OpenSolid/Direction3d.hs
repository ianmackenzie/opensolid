module OpenSolid.Direction3d
  ( Direction3d (Direction3d)
  , xComponent
  , yComponent
  , zComponent
  , components
  , unsafe
  , unwrap
  , x
  , y
  , z
  , positiveX
  , negativeX
  , positiveY
  , negativeY
  , positiveZ
  , negativeZ
  , angleFrom
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import {-# SOURCE #-} OpenSolid.Basis3d (Basis3d)
import {-# SOURCE #-} OpenSolid.Frame3d (Frame3d)
import OpenSolid.Prelude
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units (Radians)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

type role Direction3d phantom

newtype Direction3d (space :: Type) = Unit (Vector3d (space @ Unitless))
  deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Float -> Float -> Float -> Direction3d space
pattern Direction3d dx dy dz <- Unit (Vector3d dx dy dz)

instance HasUnits (Direction3d space) Unitless

instance space1 ~ space2 => Units.Coercion (Direction3d space1) (Direction3d space2) where
  coerce = identity

instance
  space1 ~ space2 =>
  ApproximateEquality (Direction3d space1) (Direction3d space2) Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Angle.zero

instance Negation (Direction3d space) where
  negate (Unit vector) = unsafe (negate vector)

instance Multiplication Sign (Direction3d space) (Direction3d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication' Sign (Direction3d space) (Direction3d space) where
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Multiplication (Direction3d space) Sign (Direction3d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance Multiplication' (Direction3d space) Sign (Direction3d space) where
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance Multiplication (Qty units) (Direction3d space) (Vector3d (space @ units)) where
  scale * Unit vector = scale * vector

instance
  Multiplication'
    (Qty units)
    (Direction3d space)
    (Vector3d (space @ (units :*: Unitless)))
  where
  scale .*. Unit vector = scale .*. vector

instance Multiplication (Direction3d space) (Qty units) (Vector3d (space @ units)) where
  Unit vector * scale = vector * scale

instance
  Multiplication'
    (Direction3d space)
    (Qty units)
    (Vector3d (space @ (Unitless :*: units)))
  where
  Unit vector .*. scale = vector .*. scale

instance space1 ~ space2 => DotMultiplication (Direction3d space1) (Direction3d space2) Float where
  Unit vector1 <> Unit vector2 = vector1 <> vector2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction3d space1)
    (Direction3d space2)
    (Qty (Unitless :*: Unitless))
  where
  Unit vector1 .<>. Unit vector2 = vector1 .<>. vector2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3d space1) (Direction3d space2) (Vector3d (space1 @ Unitless))
  where
  Unit vector1 >< Unit vector2 = vector1 >< vector2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction3d space1)
    (Direction3d space2)
    (Vector3d (space1 @ (Unitless :*: Unitless)))
  where
  Unit vector1 .><. Unit vector2 = vector1 .><. vector2

xComponent :: Direction3d space -> Float
xComponent (Unit vector) = Vector3d.xComponent vector

yComponent :: Direction3d space -> Float
yComponent (Unit vector) = Vector3d.yComponent vector

zComponent :: Direction3d space -> Float
zComponent (Unit vector) = Vector3d.zComponent vector

{-# INLINE components #-}
components :: Direction3d space -> (Float, Float, Float)
components direction = Vector3d.components (unwrap direction)

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unsafe = Unit

unwrap :: Direction3d space -> Vector3d (space @ Unitless)
unwrap (Unit vector) = vector

{-# INLINE lift #-}
lift ::
  (Vector3d (spaceA @ Unitless) -> Vector3d (spaceB @ Unitless)) ->
  Direction3d spaceA ->
  Direction3d spaceB
lift function (Unit vector) = Unit (function vector)

positiveX :: Direction3d space
positiveX = unsafe (Vector3d 1.0 0.0 0.0)

negativeX :: Direction3d space
negativeX = negate positiveX

positiveY :: Direction3d space
positiveY = unsafe (Vector3d 0.0 1.0 0.0)

negativeY :: Direction3d space
negativeY = negate positiveY

positiveZ :: Direction3d space
positiveZ = unsafe (Vector3d 0.0 0.0 1.0)

negativeZ :: Direction3d space
negativeZ = negate positiveZ

x :: Direction3d space
x = positiveX

y :: Direction3d space
y = positiveY

z :: Direction3d space
z = positiveZ

angleFrom :: Direction3d space -> Direction3d space -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3d.magnitude (d1 >< d2)) (d1 <> d2)

placeIn :: Frame3d (global @ originUnits) (Defines local) -> Direction3d local -> Direction3d global
placeIn frame = lift (Vector3d.placeIn frame)

relativeTo :: Frame3d (global @ originUnits) (Defines local) -> Direction3d global -> Direction3d local
relativeTo frame = lift (Vector3d.relativeTo frame)

placeInBasis :: Basis3d global (Defines local) -> Direction3d local -> Direction3d global
placeInBasis basis = lift (Vector3d.placeInBasis basis)

relativeToBasis :: Basis3d global (Defines local) -> Direction3d global -> Direction3d local
relativeToBasis basis = lift (Vector3d.relativeToBasis basis)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ translationUnits) ->
  Direction3d space ->
  Direction3d space
transformBy transform = lift (Vector3d.transformBy transform)
