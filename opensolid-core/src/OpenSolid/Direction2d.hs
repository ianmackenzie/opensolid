module OpenSolid.Direction2d
  ( Direction2d (Direction2d)
  , unwrap
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
  , random
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import {-# SOURCE #-} OpenSolid.Axis2d (Axis2d)
import {-# SOURCE #-} OpenSolid.Basis2d (Basis2d)
import OpenSolid.Error qualified as Error
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import {-# SOURCE #-} OpenSolid.Frame2d (Frame2d)
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units (Radians)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d, Vector2d#))
import OpenSolid.Vector2d qualified as Vector2d

type role Direction2d phantom

{-| A direction in 2D.

This is effectively a type-safe unit vector.
-}
newtype Direction2d (space :: Type) = Unit (Vector2d (space @ Unitless))
  deriving (Eq, Show)

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Float -> Float -> Direction2d space
pattern Direction2d dx dy <- Unit (Vector2d dx dy)

instance HasUnits (Direction2d space) Unitless (Direction2d space)

instance FFI (Direction2d space) where
  representation = FFI.classRepresentation "Direction2d"

instance space1 ~ space2 => Units.Coercion (Direction2d space1) (Direction2d space2) where
  coerce = identity

instance
  space1 ~ space2 =>
  ApproximateEquality (Direction2d space1) (Direction2d space2) Radians
  where
  d1 ~= d2 = angleFrom d1 d2 ~= Angle.zero

instance Negation (Direction2d space) where
  negate direction = Unit -(unwrap direction)

instance Multiplication' Sign (Direction2d space) (Direction2d space) where
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Multiplication Sign (Direction2d space) (Direction2d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication' (Direction2d space) Sign (Direction2d space) where
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance Multiplication (Direction2d space) Sign (Direction2d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance
  Multiplication'
    (Qty units)
    (Direction2d space)
    (Vector2d (space @ (units :*: Unitless)))
  where
  scale .*. direction = scale .*. unwrap direction

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units)) where
  scale * direction = scale * unwrap direction

instance
  Multiplication'
    (Direction2d space)
    (Qty units)
    (Vector2d (space @ (Unitless :*: units)))
  where
  direction .*. scale = unwrap direction .*. scale

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units)) where
  direction * scale = unwrap direction * scale

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Direction2d space2) (Qty (Unitless :*: Unitless))
  where
  Unit v1 .<>. Unit v2 = v1 .<>. v2

instance space1 ~ space2 => DotMultiplication (Direction2d space1) (Direction2d space2) Float where
  Unit v1 <> Unit v2 = v1 <> v2

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Direction2d space2) (Qty (Unitless :*: Unitless))
  where
  Unit v1 .><. Unit v2 = v1 .><. v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Direction2d space2) Float
  where
  Unit v1 >< Unit v2 = v1 >< v2

{-# INLINE unwrap #-}
unwrap :: Direction2d space -> Vector2d (space @ Unitless)
unwrap (Unit v) = v

-- | Get the X component of a direction.
xComponent :: Direction2d space -> Float
xComponent direction = Vector2d.xComponent (unwrap direction)

-- | Get the Y component of a direction.
yComponent :: Direction2d space -> Float
yComponent direction = Vector2d.yComponent (unwrap direction)

-- | Get the X and Y components of a direction.
{-# INLINE components #-}
components :: Direction2d space -> (Float, Float)
components direction = Vector2d.components (unwrap direction)

{-# INLINE unsafe #-}
unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unsafe = Unit

{-# INLINE lift #-}
lift ::
  (Vector2d (spaceA @ Unitless) -> Vector2d (spaceB @ Unitless)) ->
  Direction2d spaceA ->
  Direction2d spaceB
lift function (Unit vector) = Unit (function vector)

-- | The positive X direction.
positiveX :: Direction2d space
positiveX = unsafe (Vector2d# 1.0## 0.0##)

-- | The negative X direction.
negativeX :: Direction2d space
negativeX = unsafe (Vector2d# -1.0## 0.0##)

-- | The positive Y direction.
positiveY :: Direction2d space
positiveY = unsafe (Vector2d# 0.0## 1.0##)

-- | The negative Y direction.
negativeY :: Direction2d space
negativeY = unsafe (Vector2d# 0.0## -1.0##)

-- | Alias for 'positiveX'.
x :: Direction2d space
x = positiveX

-- | Alias for 'positiveY'.
y :: Direction2d space
y = positiveY

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show, Error.Message)

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result PointsAreCoincident (Direction2d space)
from p1 p2 =
  case Vector2d.direction (p2 - p1) of
    Success direction -> Success direction
    Failure Vector2d.IsZero -> Failure PointsAreCoincident

{-| Construct a direction from an angle.

The angle is measured counterclockwise from the positive X direction, so:

  * An angle of zero corresponds to the positive X direction
  * An angle of 90 degrees corresponds to the positive Y direction
  * An angle of 180 degrees (or -180 degrees) corresponds to the negative X direction
-}
fromAngle :: Angle -> Direction2d space
fromAngle angle = unsafe (Vector2d.polar 1.0 angle)

{-| Convert a direction to an angle.

The angle is measured counterclockwise from the positive X direction, so:

  * The positive X direction has an angle of zero.
  * The positive Y direction has an angle of 90 degrees.
  * The negative Y direction has an angle of -90 degrees.
  * It is not defined whether the negative X direction has an angle of -180 or
    +180 degrees. (Currently it is reported as having an angle of +180 degrees,
    but this should not be relied upon.)

The returned angle will be between -180 and +180 degrees.
-}
toAngle :: Direction2d space -> Angle
toAngle (Unit vector) = Vector2d.angle vector

{-| Construct a direction from an angle given in degrees.

See 'fromAngle' for details.
-}
degrees :: Float -> Direction2d space
degrees value = fromAngle (Angle.degrees value)

{-| Construct a direction from an angle given in radians.

See 'fromAngle' for details.
-}
radians :: Float -> Direction2d space
radians value = fromAngle (Angle.radians value)

{-| Measure the signed angle from one direction to another.

The angle will be measured counterclockwise from the first direction to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Direction2d space -> Direction2d space -> Angle
angleFrom d1 d2 = Angle.atan2 (d1 >< d2) (d1 <> d2)

perpendicularTo :: Direction2d space -> Direction2d space
perpendicularTo = rotateLeft

rotateLeft :: Direction2d space -> Direction2d space
rotateLeft = lift Vector2d.rotateLeft

rotateRight :: Direction2d space -> Direction2d space
rotateRight = lift Vector2d.rotateRight

placeIn :: Frame2d (global @ originUnits) (Defines local) -> Direction2d local -> Direction2d global
placeIn frame = lift (Vector2d.placeIn frame)

relativeTo :: Frame2d (global @ originUnits) (Defines local) -> Direction2d global -> Direction2d local
relativeTo frame = lift (Vector2d.relativeTo frame)

placeInBasis :: Basis2d global (Defines local) -> Direction2d local -> Direction2d global
placeInBasis basis = lift (Vector2d.placeInBasis basis)

relativeToBasis :: Basis2d global (Defines local) -> Direction2d global -> Direction2d local
relativeToBasis basis = lift (Vector2d.relativeToBasis basis)

random :: Random.Generator (Direction2d space)
random = Random.map fromAngle (Qty.random -Angle.pi Angle.pi)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2d tag (space @ translationUnits) ->
  Direction2d space ->
  Direction2d space
transformBy transform = lift (Vector2d.transformBy transform)

rotateBy :: Angle -> Direction2d space -> Direction2d space
rotateBy theta = lift (Vector2d.rotateBy theta)

mirrorIn :: Direction2d space -> Direction2d space -> Direction2d space
mirrorIn mirrorDirection = lift (Vector2d.mirrorIn mirrorDirection)

mirrorAcross :: Axis2d (space @ originUnits) -> Direction2d space -> Direction2d space
mirrorAcross axis = lift (Vector2d.mirrorAcross axis)
