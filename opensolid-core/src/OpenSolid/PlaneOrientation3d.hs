module OpenSolid.PlaneOrientation3d
  ( PlaneOrientation3d
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

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3d (Unit3d)
  , Frame3d
  , PlaneOrientation3d (PlaneOrientation3d)
  , Transform3d
  , Vector3d
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3d qualified as Vector3d

coerce :: PlaneOrientation3d space1 -> PlaneOrientation3d space2
coerce (PlaneOrientation3d i j) = PlaneOrientation3d (Direction3d.coerce i) (Direction3d.coerce j)

unsafe :: Direction3d space -> Direction3d space -> PlaneOrientation3d space
unsafe = PlaneOrientation3d

{-| Construct a plane orientation normal to the given direction.

Both the X and Y directions of the returned orientation will be perpendicular to the given direction
(and, of course, they will be perpendicular to each other),
but otherwise they will be chosen arbitrarily.
-}
fromNormalDirection :: Direction3d space -> PlaneOrientation3d space
fromNormalDirection n = do
  let x = Direction3d.perpendicularDirection n
  let y = Unit3d (n `cross` x)
  PlaneOrientation3d x y

{-| Construct a plane orientation from its X direction.

The Y direction of the returned basis will be perpendicular to the given X direction,
but otherwise will be chosen arbitrarily.
-}
fromXDirection :: Direction3d space -> PlaneOrientation3d space
fromXDirection dx = PlaneOrientation3d dx (Direction3d.perpendicularDirection dx)

{-| Construct a plane orientation from its Y direction.

The X direction of the returned basis will be perpendicular to the given Y direction,
but otherwise will be chosen arbitrarily.
-}
fromYDirection :: Direction3d space -> PlaneOrientation3d space
fromYDirection dy = PlaneOrientation3d (Direction3d.perpendicularDirection dy) dy

fromDirections ::
  Tolerance Unitless =>
  Direction3d space ->
  Direction3d space ->
  Maybe (PlaneOrientation3d space)
fromDirections dx dxy = gramSchmidt dx (Vector3d.unit dxy)

fromVectors ::
  Tolerance units =>
  Vector3d space units ->
  Vector3d space units ->
  Maybe (PlaneOrientation3d space)
fromVectors vx vxy =
  case Vector3d.direction vx of
    Error Vector3d.IsZero -> Nothing
    Ok dx -> gramSchmidt dx vxy

gramSchmidt ::
  Tolerance units =>
  Direction3d space ->
  Vector3d space units ->
  Maybe (PlaneOrientation3d space)
gramSchmidt dx vxy = do
  let vy = vxy .-. Vector3d.projectionIn dx vxy
  case Vector3d.direction vy of
    Error Vector3d.IsZero -> Nothing
    Ok dy -> Just (PlaneOrientation3d dx dy)

flip :: PlaneOrientation3d space -> PlaneOrientation3d space
flip (PlaneOrientation3d i j) = PlaneOrientation3d (negative i) j

-- | Get the X direction of a plane orientation.
xDirection :: PlaneOrientation3d space -> Direction3d space
xDirection = (.xDirection)

-- | Get the Y direction of a plane orientation.
yDirection :: PlaneOrientation3d space -> Direction3d space
yDirection = (.yDirection)

-- | Get the normal (outward) direction of a plane orientation.
normalDirection :: PlaneOrientation3d space -> Direction3d space
normalDirection = (.normalDirection)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag space translationUnits ->
  PlaneOrientation3d space ->
  PlaneOrientation3d space
transformBy transform (PlaneOrientation3d i j) =
  PlaneOrientation3d
    (Direction3d.transformBy transform i)
    (Direction3d.transformBy transform j)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d global frameUnits (Defines local) ->
  PlaneOrientation3d local ->
  PlaneOrientation3d global
placeIn globalOrientation (PlaneOrientation3d i j) =
  PlaneOrientation3d
    (Direction3d.placeIn globalOrientation i)
    (Direction3d.placeIn globalOrientation j)

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d global frameUnits (Defines local) ->
  PlaneOrientation3d global ->
  PlaneOrientation3d local
relativeTo globalOrientation (PlaneOrientation3d i j) =
  PlaneOrientation3d
    (Direction3d.relativeTo globalOrientation i)
    (Direction3d.relativeTo globalOrientation j)

-- | Generate a random plane orientation.
random :: Random.Generator (PlaneOrientation3d global)
random =
  Random.retry do
    Random.map2
      (Tolerance.using 0.1 fromDirections)
      Direction3d.random
      Direction3d.random
