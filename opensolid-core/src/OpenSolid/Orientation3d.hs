module OpenSolid.Orientation3d
  ( Orientation3d
  , world
  , coerce
  , upwardDirection
  , downwardDirection
  , forwardDirection
  , backwardDirection
  , rightwardDirection
  , leftwardDirection
  , topPlaneOrientation
  , bottomPlaneOrientation
  , frontPlaneOrientation
  , backPlaneOrientation
  , rightPlaneOrientation
  , leftPlaneOrientation
  , fromTopPlaneOrientation
  , fromBottomPlaneOrientation
  , fromFrontPlaneOrientation
  , fromBackPlaneOrientation
  , fromRightPlaneOrientation
  , fromLeftPlaneOrientation
  , forwardOrientation
  , backwardOrientation
  , leftwardOrientation
  , rightwardOrientation
  , upwardOrientation
  , downwardOrientation
  , transformBy
  , placeIn
  , relativeTo
  , inverse
  , random
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Direction3d (Direction3d, Unit3d)
  , Orientation3d (Orientation3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Transform3d qualified as Transform3d

{-| The global orientation of the current coordinate space.

That is, the forward direction of this orientation is the global forward direction,
the upward direction of this orientation is the global upward direction, etc.
-}
world :: Orientation3d space (Defines space)
world =
  Orientation3d
    # Direction3d 1.0 0.0 0.0
    # Direction3d 0.0 1.0 0.0
    # Direction3d 0.0 0.0 1.0

coerce :: Orientation3d space1 defines1 -> Orientation3d space2 defines2
coerce (Orientation3d rightward forward upward) =
  Orientation3d
    # Direction3d.coerce rightward
    # Direction3d.coerce forward
    # Direction3d.coerce upward

-- | Get the rightward direction of a orientation.
rightwardDirection :: Orientation3d space defines -> Direction3d space
rightwardDirection (Orientation3d r _ _) = r

-- | Get the forward direction of a orientation.
forwardDirection :: Orientation3d space defines -> Direction3d space
forwardDirection (Orientation3d _ f _) = f

-- | Get the upward direction of a orientation.
upwardDirection :: Orientation3d space defines -> Direction3d space
upwardDirection (Orientation3d _ _ u) = u

-- | Get the leftward direction of a orientation.
leftwardDirection :: Orientation3d space defines -> Direction3d space
leftwardDirection = negate . rightwardDirection

-- | Get the backward direction of a orientation.
backwardDirection :: Orientation3d space defines -> Direction3d space
backwardDirection = negate . forwardDirection

-- | Get the downward direction of a orientation.
downwardDirection :: Orientation3d space defines -> Direction3d space
downwardDirection = negate . upwardDirection

{-| Construct a forward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point forward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point upward.
-}
frontPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
frontPlaneOrientation orientation =
  PlaneOrientation3d (leftwardDirection orientation) (upwardDirection orientation)

{-| Construct a backward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point backward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point upward.
-}
backPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
backPlaneOrientation orientation =
  PlaneOrientation3d (rightwardDirection orientation) (upwardDirection orientation)

{-| Construct a leftward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point leftward,
the X direction of the plane orientation will point backward,
and the Y direction of the plane orientation will point upward.
-}
leftPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
leftPlaneOrientation orientation =
  PlaneOrientation3d (backwardDirection orientation) (upwardDirection orientation)

{-| Construct a rightward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point rightward,
the X direction of the plane orientation will point forward,
and the Y direction of the plane orientation will point upward.
-}
rightPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
rightPlaneOrientation orientation =
  PlaneOrientation3d (forwardDirection orientation) (upwardDirection orientation)

{-| Construct a upward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point upward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point forward.
-}
topPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
topPlaneOrientation orientation =
  PlaneOrientation3d (rightwardDirection orientation) (forwardDirection orientation)

{-| Construct a downward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point downward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point forward.
-}
bottomPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
bottomPlaneOrientation orientation =
  PlaneOrientation3d (leftwardDirection orientation) (forwardDirection orientation)

-- | Construct an orientation from its front plane orientation.
fromFrontPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromFrontPlaneOrientation (PlaneOrientation3d l u) = Orientation3d -l (Unit3d (l `cross` u)) u

-- | Construct an orientation from its back plane orientation.
fromBackPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromBackPlaneOrientation (PlaneOrientation3d r u) = Orientation3d r (Unit3d (u `cross` r)) u

-- | Construct an orientation from its left plane orientation.
fromLeftPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromLeftPlaneOrientation (PlaneOrientation3d b u) = Orientation3d (Unit3d (u `cross` b)) -b u

-- | Construct an orientation from its right plane orientation.
fromRightPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromRightPlaneOrientation (PlaneOrientation3d f u) = Orientation3d (Unit3d (f `cross` u)) f u

-- | Construct an orientation from its top plane orientation.
fromTopPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromTopPlaneOrientation (PlaneOrientation3d r f) = Orientation3d r f (Unit3d (r `cross` f))

-- | Construct an orientation from its bottom plane orientation.
fromBottomPlaneOrientation :: PlaneOrientation3d space defines1 -> Orientation3d space defines2
fromBottomPlaneOrientation (PlaneOrientation3d l f) = Orientation3d -l f (Unit3d (f `cross` l))

{-| Construct a forward facing orientation relative to a parent/reference orientation.

This is a new orientation with the same orientation as the parent orientation.
-}
forwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
forwardOrientation (Orientation3d r f u) = Orientation3d r f u

{-| Construct a backward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point backward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point upward.
-}
backwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
backwardOrientation (Orientation3d r f u) = Orientation3d -r -f u

{-| Construct a leftward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point leftward,
the rightward direction of the orientation will point forward,
and the upward direction of the orientation will point upward.
-}
leftwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
leftwardOrientation (Orientation3d r f u) = Orientation3d f -r u

{-| Construct a rightward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point rightward,
the rightward direction of the orientation will point backward,
and the upward direction of the orientation will point upward.
-}
rightwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
rightwardOrientation (Orientation3d r f u) = Orientation3d -f r u

{-| Construct an upward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point upward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point forward.
-}
upwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
upwardOrientation (Orientation3d r f u) = Orientation3d -r u f

{-| Construct a downward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point downward,
the rightward direction of the orientation will point rightward,
and the upward direction of the orientation will point forward.
-}
downwardOrientation :: Orientation3d space defines1 -> Orientation3d space defines2
downwardOrientation (Orientation3d r f u) = Orientation3d r -u f

transformBy ::
  Transform3d.Rigid (space @ translationUnits) ->
  Orientation3d space defines1 ->
  Orientation3d space defines2
transformBy transform (Orientation3d i j k) =
  Orientation3d
    (Direction3d.transformBy transform i)
    (Direction3d.transformBy transform j)
    (Direction3d.transformBy transform k)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn ::
  Orientation3d global (Defines local) ->
  Orientation3d local defines ->
  Orientation3d global defines
placeIn globalOrientation (Orientation3d i j k) =
  Orientation3d
    (Direction3d.placeIn globalOrientation i)
    (Direction3d.placeIn globalOrientation j)
    (Direction3d.placeIn globalOrientation k)

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Orientation3d global (Defines local) ->
  Orientation3d global defines ->
  Orientation3d local defines
relativeTo globalOrientation (Orientation3d i j k) =
  Orientation3d
    (Direction3d.relativeTo globalOrientation i)
    (Direction3d.relativeTo globalOrientation j)
    (Direction3d.relativeTo globalOrientation k)

inverse :: Orientation3d global (Defines local) -> Orientation3d local (Defines global)
inverse orientation = world |> relativeTo orientation

random :: Random.Generator (Orientation3d local (Defines global))
random = Random.map fromTopPlaneOrientation PlaneOrientation3d.random
