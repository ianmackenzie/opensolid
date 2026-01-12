module OpenSolid.PlaneOrientation3D
  ( PlaneOrientation3D
  , coerce
  , unsafe
  , fromNormalDirection
  , fromXDirection
  , fromYDirection
  , fromDirections
  , fromVectors
  , flip
  , xDirection
  , yDirection
  , normalDirection
  , transformBy
  , placeIn
  , relativeTo
  , random
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3D (Unit3D)
  , Frame3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Transform3D
  , Vector3D
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3D qualified as Vector3D

coerce :: PlaneOrientation3D space1 -> PlaneOrientation3D space2
coerce (PlaneOrientation3D i j) = PlaneOrientation3D (Direction3D.coerce i) (Direction3D.coerce j)

unsafe :: Direction3D space -> Direction3D space -> PlaneOrientation3D space
unsafe = PlaneOrientation3D

{-| Construct a plane orientation normal to the given direction.

Both the X and Y directions of the returned orientation will be perpendicular to the given direction
(and, of course, they will be perpendicular to each other),
but otherwise they will be chosen arbitrarily.
-}
fromNormalDirection :: Direction3D space -> PlaneOrientation3D space
fromNormalDirection n = do
  let x = Direction3D.perpendicularDirection n
  let y = Unit3D (n `cross` x)
  PlaneOrientation3D x y

{-| Construct a plane orientation from its X direction.

The Y direction of the returned basis will be perpendicular to the given X direction,
but otherwise will be chosen arbitrarily.
-}
fromXDirection :: Direction3D space -> PlaneOrientation3D space
fromXDirection dx = PlaneOrientation3D dx (Direction3D.perpendicularDirection dx)

{-| Construct a plane orientation from its Y direction.

The X direction of the returned basis will be perpendicular to the given Y direction,
but otherwise will be chosen arbitrarily.
-}
fromYDirection :: Direction3D space -> PlaneOrientation3D space
fromYDirection dy = PlaneOrientation3D (Direction3D.perpendicularDirection dy) dy

fromDirections ::
  Tolerance Unitless =>
  Direction3D space ->
  Direction3D space ->
  Maybe (PlaneOrientation3D space)
fromDirections dx dxy = gramSchmidt dx (Vector3D.unit dxy)

fromVectors ::
  Tolerance units =>
  Vector3D units space ->
  Vector3D units space ->
  Maybe (PlaneOrientation3D space)
fromVectors vx vxy =
  case Vector3D.direction vx of
    Error Vector3D.IsZero -> Nothing
    Ok dx -> gramSchmidt dx vxy

gramSchmidt ::
  Tolerance units =>
  Direction3D space ->
  Vector3D units space ->
  Maybe (PlaneOrientation3D space)
gramSchmidt dx vxy = do
  let vy = vxy .-. Vector3D.projectionIn dx vxy
  case Vector3D.direction vy of
    Error Vector3D.IsZero -> Nothing
    Ok dy -> Just (PlaneOrientation3D dx dy)

flip :: PlaneOrientation3D space -> PlaneOrientation3D space
flip (PlaneOrientation3D i j) = PlaneOrientation3D (negative i) j

-- | Get the X direction of a plane orientation.
xDirection :: PlaneOrientation3D space -> Direction3D space
xDirection = (.xDirection)

-- | Get the Y direction of a plane orientation.
yDirection :: PlaneOrientation3D space -> Direction3D space
yDirection = (.yDirection)

-- | Get the normal (outward) direction of a plane orientation.
normalDirection :: PlaneOrientation3D space -> Direction3D space
normalDirection = (.normalDirection)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3D tag space ->
  PlaneOrientation3D space ->
  PlaneOrientation3D space
transformBy transform (PlaneOrientation3D i j) =
  PlaneOrientation3D
    (Direction3D.transformBy transform i)
    (Direction3D.transformBy transform j)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> PlaneOrientation3D local -> PlaneOrientation3D global
placeIn globalOrientation (PlaneOrientation3D i j) =
  PlaneOrientation3D
    (Direction3D.placeIn globalOrientation i)
    (Direction3D.placeIn globalOrientation j)

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> PlaneOrientation3D global -> PlaneOrientation3D local
relativeTo globalOrientation (PlaneOrientation3D i j) =
  PlaneOrientation3D
    (Direction3D.relativeTo globalOrientation i)
    (Direction3D.relativeTo globalOrientation j)

-- | Generate a random plane orientation.
random :: Random.Generator (PlaneOrientation3D global)
random =
  Random.retry do
    Random.map2
      (Tolerance.using 0.1 fromDirections)
      Direction3D.random
      Direction3D.random
