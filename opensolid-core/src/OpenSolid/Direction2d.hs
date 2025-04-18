module OpenSolid.Direction2d
  ( Direction2d (Direction2d)
  , xComponent
  , yComponent
  , components
  , unsafe
  , coerce
  , x
  , y
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
  , placeOn
  , random
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Error qualified as Error
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d
  , Basis2d
  , Direction2d (Unit2d)
  , Direction3d (Unit3d)
  , PlanarBasis3d
  , Point2d
  , Transform2d
  , Vector2d (Vector2d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector2d qualified as Vector2d

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Float -> Float -> Direction2d space
pattern Direction2d dx dy <- Unit2d (Vector2d dx dy)

-- | Get the X component of a direction.
xComponent :: Direction2d space -> Float
xComponent (Direction2d dx _) = dx

-- | Get the Y component of a direction.
yComponent :: Direction2d space -> Float
yComponent (Direction2d _ dy) = dy

-- | Get the XY components of a direction as a tuple.
{-# INLINE components #-}
components :: Direction2d space -> (Float, Float)
components (Direction2d dx dy) = (dx, dy)

{-# INLINE unsafe #-}
unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unsafe = Unit2d

{-# INLINE coerce #-}
coerce :: Direction2d space1 -> Direction2d space2
coerce (Unit2d (Vector2d dx dy)) = Unit2d (Vector2d dx dy)

{-# INLINE lift #-}
lift ::
  (Vector2d (space1 @ Unitless) -> Vector2d (space2 @ Unitless)) ->
  Direction2d space1 ->
  Direction2d space2
lift function (Unit2d vector) = Unit2d (function vector)

-- | The X direction.
x :: Direction2d space
x = Unit2d (Vector2d 1.0 0.0)

-- | The Y direction.
y :: Direction2d space
y = Unit2d (Vector2d 0.0 1.0)

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show, Error.Message)

from ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result PointsAreCoincident (Direction2d space)
from p1 p2 = do
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
fromAngle angle = Unit2d (Vector2d (Angle.cos angle) (Angle.sin angle))

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
toAngle (Unit2d vector) = Vector2d.angle vector

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
angleFrom d1 d2 = Angle.atan2 (d1 `cross` d2) (d1 `dot` d2)

perpendicularTo :: Direction2d space -> Direction2d space
perpendicularTo = rotateLeft

rotateLeft :: Direction2d space -> Direction2d space
rotateLeft = lift Vector2d.rotateLeft

rotateRight :: Direction2d space -> Direction2d space
rotateRight = lift Vector2d.rotateRight

placeIn :: Basis2d global (Defines local) -> Direction2d local -> Direction2d global
placeIn basis = lift (Vector2d.placeIn basis)

relativeTo :: Basis2d global (Defines local) -> Direction2d global -> Direction2d local
relativeTo basis = lift (Vector2d.relativeTo basis)

placeOn ::
  PlanarBasis3d space (Defines local) ->
  Direction2d local ->
  Direction3d space
placeOn planarBasis (Unit2d vector) = Unit3d (Vector2d.placeOn planarBasis vector)

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
