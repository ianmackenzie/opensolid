module OpenSolid.PlaneOrientation3d
  ( PlaneOrientation3d
  , coerce
  , unsafe
  , arbitraryNormalOrientation
  , withArbitraryYDirection
  , withArbitraryXDirection
  , orthogonalize
  , orthonormalize
  , flip
  , xDirection
  , yDirection
  , normalDirection
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Direction3d (Direction3d, arbitraryNormalOrientation)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3d (Unit3d)
  , Orientation3d
  , PlaneOrientation3d (PlaneOrientation3d)
  , Transform3d
  , Vector3d
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3d qualified as Vector3d

coerce :: PlaneOrientation3d space1 defines1 -> PlaneOrientation3d space2 defines2
coerce (PlaneOrientation3d i j) = PlaneOrientation3d (Direction3d.coerce i) (Direction3d.coerce j)

unsafe :: Direction3d space -> Direction3d space -> PlaneOrientation3d space defines
unsafe = PlaneOrientation3d

withArbitraryYDirection :: Named "xDirection" (Direction3d space) -> PlaneOrientation3d space defines
withArbitraryYDirection (Named dx) =
  PlaneOrientation3d dx (Direction3d.arbitraryPerpendicularDirection dx)

withArbitraryXDirection :: Named "yDirection" (Direction3d space) -> PlaneOrientation3d space defines
withArbitraryXDirection (Named dy) =
  PlaneOrientation3d (Direction3d.arbitraryPerpendicularDirection dy) dy

orthogonalize ::
  Tolerance Unitless =>
  Direction3d space ->
  Direction3d space ->
  Maybe (PlaneOrientation3d space defines)
orthogonalize dx dxy = gramSchmidt dx (Vector3d.unit dxy)

orthonormalize ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Maybe (PlaneOrientation3d space defines)
orthonormalize vx vxy =
  case Vector3d.direction vx of
    Failure Vector3d.IsZero -> Nothing
    Success dx -> gramSchmidt dx vxy

gramSchmidt ::
  Tolerance units =>
  Direction3d space ->
  Vector3d (space @ units) ->
  Maybe (PlaneOrientation3d space defines)
gramSchmidt dx vxy = do
  let vy = vxy - Vector3d.projectionIn dx vxy
  case Vector3d.direction vy of
    Failure Vector3d.IsZero -> Nothing
    Success dy -> Just (PlaneOrientation3d dx dy)

flip :: PlaneOrientation3d space defines1 -> PlaneOrientation3d space defines2
flip (PlaneOrientation3d i j) = PlaneOrientation3d -i j

-- | Get the X direction of a plane orientation.
xDirection :: PlaneOrientation3d space defines -> Direction3d space
xDirection (PlaneOrientation3d i _) = i

-- | Get the Y direction of a plane orientation.
yDirection :: PlaneOrientation3d space defines -> Direction3d space
yDirection (PlaneOrientation3d _ j) = j

-- | Get the normal (outward) direction of a plane orientation.
normalDirection :: PlaneOrientation3d space defines -> Direction3d space
normalDirection (PlaneOrientation3d i j) = Unit3d (i `cross` j)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ translationUnits) ->
  PlaneOrientation3d space defines ->
  PlaneOrientation3d space defines
transformBy transform (PlaneOrientation3d i j) =
  PlaneOrientation3d (Direction3d.transformBy transform i) (Direction3d.transformBy transform j)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn ::
  Orientation3d global (Defines local) ->
  PlaneOrientation3d local defines ->
  PlaneOrientation3d global defines
placeIn globalOrientation (PlaneOrientation3d i j) =
  PlaneOrientation3d (Direction3d.placeIn globalOrientation i) (Direction3d.placeIn globalOrientation j)

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Orientation3d global (Defines local) ->
  PlaneOrientation3d global defines ->
  PlaneOrientation3d local defines
relativeTo globalOrientation (PlaneOrientation3d i j) =
  PlaneOrientation3d (Direction3d.relativeTo globalOrientation i) (Direction3d.relativeTo globalOrientation j)
