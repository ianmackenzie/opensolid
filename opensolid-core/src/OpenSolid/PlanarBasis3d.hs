module OpenSolid.PlanarBasis3d
  ( PlanarBasis3d
  , coerce
  , unsafe
  , forwardFacing
  , backwardFacing
  , leftwardFacing
  , rightwardFacing
  , upwardFacing
  , downwardFacing
  , arbitraryNormalBasis
  , withArbitraryYDirection
  , withArbitraryXDirection
  , orthogonalize
  , orthonormalize
  , flipX
  , flipY
  , xDirection
  , yDirection
  , normalDirection
  , xnBasis
  , nxBasis
  , ynBasis
  , nyBasis
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Direction3d (Direction3d, arbitraryNormalBasis)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Basis3d
  , Direction3d (Unit3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Transform3d
  , Vector3d
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3d qualified as Vector3d

coerce :: PlanarBasis3d space1 defines1 -> PlanarBasis3d space2 defines2
coerce (PlanarBasis3d i j) = PlanarBasis3d (Direction3d.coerce i) (Direction3d.coerce j)

unsafe :: Direction3d space -> Direction3d space -> PlanarBasis3d space defines
unsafe = PlanarBasis3d

{-| The basis for a forward-facing plane.

The normal direction of the basis will point forward,
the X direction of the basis will point leftward,
and the Y direction of the basis will point upward.
-}
forwardFacing :: PlanarBasis3d space defines
forwardFacing = PlanarBasis3d Direction3d.leftward Direction3d.upward

{-| The basis for a backward-facing plane.

The normal direction of the basis will point backward,
the X direction of the basis will point rightward,
and the Y direction of the basis will point upward.
-}
backwardFacing :: PlanarBasis3d space defines
backwardFacing = PlanarBasis3d Direction3d.rightward Direction3d.upward

{-| The basis for a leftward-facing plane.

The normal direction of the basis will point leftward,
the X direction of the basis will point backward,
and the Y direction of the basis will point upward.
-}
leftwardFacing :: PlanarBasis3d space defines
leftwardFacing = PlanarBasis3d Direction3d.backward Direction3d.upward

{-| The basis for a rightward-facing plane.

The normal direction of the basis will point rightward,
the X direction of the basis will point forward,
and the Y direction of the basis will point upward.
-}
rightwardFacing :: PlanarBasis3d space defines
rightwardFacing = PlanarBasis3d Direction3d.forward Direction3d.upward

{-| The basis for an upward-facing plane.

The normal direction of the basis will point upward,
the X direction of the basis will point rightward,
and the Y direction of the basis will point forward.
-}
upwardFacing :: PlanarBasis3d space defines
upwardFacing = PlanarBasis3d Direction3d.rightward Direction3d.forward

{-| The basis for a downward-facing plane.

The normal direction of the basis will point downward,
the X direction of the basis will point leftward,
and the Y direction of the basis will point forward.
-}
downwardFacing :: PlanarBasis3d space defines
downwardFacing = PlanarBasis3d Direction3d.leftward Direction3d.forward

withArbitraryYDirection :: Named "xDirection" (Direction3d space) -> PlanarBasis3d space defines
withArbitraryYDirection (Named dx) =
  PlanarBasis3d dx (Direction3d.arbitraryPerpendicularDirection dx)

withArbitraryXDirection :: Named "yDirection" (Direction3d space) -> PlanarBasis3d space defines
withArbitraryXDirection (Named dy) =
  PlanarBasis3d (Direction3d.arbitraryPerpendicularDirection dy) dy

orthogonalize ::
  Tolerance Unitless =>
  Direction3d space ->
  Direction3d space ->
  Maybe (PlanarBasis3d space defines)
orthogonalize dx dxy = gramSchmidt dx (Vector3d.unit dxy)

orthonormalize ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Maybe (PlanarBasis3d space defines)
orthonormalize vx vxy =
  case Vector3d.direction vx of
    Failure Vector3d.IsZero -> Nothing
    Success dx -> gramSchmidt dx vxy

gramSchmidt ::
  Tolerance units =>
  Direction3d space ->
  Vector3d (space @ units) ->
  Maybe (PlanarBasis3d space defines)
gramSchmidt dx vxy = do
  let vy = vxy - Vector3d.projectionIn dx vxy
  case Vector3d.direction vy of
    Failure Vector3d.IsZero -> Nothing
    Success dy -> Just (PlanarBasis3d dx dy)

flipX :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
flipX (PlanarBasis3d i j) = PlanarBasis3d -i j

flipY :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
flipY (PlanarBasis3d i j) = PlanarBasis3d i -j

-- | Get the X direction of a planar basis.
xDirection :: PlanarBasis3d space defines -> Direction3d space
xDirection (PlanarBasis3d i _) = i

-- | Get the Y direction of a planar basis.
yDirection :: PlanarBasis3d space defines -> Direction3d space
yDirection (PlanarBasis3d _ j) = j

-- | Get the normal (outward) direction of a planar basis.
normalDirection :: PlanarBasis3d space defines -> Direction3d space
normalDirection (PlanarBasis3d i j) = Unit3d (i `cross` j)

xnBasis :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
xnBasis basis = PlanarBasis3d (xDirection basis) (normalDirection basis)

nxBasis :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
nxBasis basis = PlanarBasis3d (normalDirection basis) (xDirection basis)

ynBasis :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
ynBasis basis = PlanarBasis3d (yDirection basis) (normalDirection basis)

nyBasis :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
nyBasis basis = PlanarBasis3d (normalDirection basis) (yDirection basis)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ translationUnits) ->
  PlanarBasis3d space defines ->
  PlanarBasis3d space defines
transformBy transform (PlanarBasis3d i j) =
  PlanarBasis3d (Direction3d.transformBy transform i) (Direction3d.transformBy transform j)

-- | Convert a basis defined in local coordinates to one defined in global coordinates.
placeIn ::
  Basis3d global (Defines local) ->
  PlanarBasis3d local defines ->
  PlanarBasis3d global defines
placeIn globalBasis (PlanarBasis3d i j) =
  PlanarBasis3d (Direction3d.placeIn globalBasis i) (Direction3d.placeIn globalBasis j)

-- | Convert a basis defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Basis3d global (Defines local) ->
  PlanarBasis3d global defines ->
  PlanarBasis3d local defines
relativeTo globalBasis (PlanarBasis3d i j) =
  PlanarBasis3d (Direction3d.relativeTo globalBasis i) (Direction3d.relativeTo globalBasis j)
