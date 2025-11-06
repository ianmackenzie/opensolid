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
  , backwardOrientation
  , leftwardOrientation
  , rightwardOrientation
  , upwardOrientation
  , downwardOrientation
  , transformBy
  , placeIn
  , relativeTo
  , random
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3d (Direction3d, Unit3d)
  , Frame3d
  , Orientation3d (Orientation3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Transform3d qualified as Transform3d

{-| The global orientation of the current coordinate space.

That is, the forward direction of this orientation is the global forward direction,
the upward direction of this orientation is the global upward direction, etc.
-}
world :: Orientation3d space
world = Orientation3d (Direction3d 1 0 0) (Direction3d 0 1 0) (Direction3d 0 0 1)

coerce :: Orientation3d space1 -> Orientation3d space2
coerce (Orientation3d r f u) =
  Orientation3d (Direction3d.coerce r) (Direction3d.coerce f) (Direction3d.coerce u)

-- | Get the rightward direction of a orientation.
rightwardDirection :: Orientation3d space -> Direction3d space
rightwardDirection = (.rightwardDirection)

-- | Get the forward direction of a orientation.
forwardDirection :: Orientation3d space -> Direction3d space
forwardDirection = (.forwardDirection)

-- | Get the upward direction of a orientation.
upwardDirection :: Orientation3d space -> Direction3d space
upwardDirection = (.upwardDirection)

-- | Get the leftward direction of a orientation.
leftwardDirection :: Orientation3d space -> Direction3d space
leftwardDirection = (.leftwardDirection)

-- | Get the backward direction of a orientation.
backwardDirection :: Orientation3d space -> Direction3d space
backwardDirection = (.backwardDirection)

-- | Get the downward direction of a orientation.
downwardDirection :: Orientation3d space -> Direction3d space
downwardDirection = (.downwardDirection)

{-| Construct a forward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point forward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point upward.
-}
frontPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
frontPlaneOrientation = (.frontPlaneOrientation)

{-| Construct a backward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point backward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point upward.
-}
backPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
backPlaneOrientation = (.backPlaneOrientation)

{-| Construct a leftward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point leftward,
the X direction of the plane orientation will point backward,
and the Y direction of the plane orientation will point upward.
-}
leftPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
leftPlaneOrientation = (.leftPlaneOrientation)

{-| Construct a rightward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point rightward,
the X direction of the plane orientation will point forward,
and the Y direction of the plane orientation will point upward.
-}
rightPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
rightPlaneOrientation = (.rightPlaneOrientation)

{-| Construct a upward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point upward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point forward.
-}
topPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
topPlaneOrientation = (.topPlaneOrientation)

{-| Construct a downward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point downward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point forward.
-}
bottomPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
bottomPlaneOrientation = (.bottomPlaneOrientation)

-- | Construct an orientation from its front plane orientation.
fromFrontPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromFrontPlaneOrientation (PlaneOrientation3d l u) =
  Orientation3d (negative l) (Unit3d (l `cross` u)) u

-- | Construct an orientation from its back plane orientation.
fromBackPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromBackPlaneOrientation (PlaneOrientation3d r u) =
  Orientation3d r (Unit3d (u `cross` r)) u

-- | Construct an orientation from its left plane orientation.
fromLeftPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromLeftPlaneOrientation (PlaneOrientation3d b u) =
  Orientation3d (Unit3d (u `cross` b)) (negative b) u

-- | Construct an orientation from its right plane orientation.
fromRightPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromRightPlaneOrientation (PlaneOrientation3d f u) =
  Orientation3d (Unit3d (f `cross` u)) f u

-- | Construct an orientation from its top plane orientation.
fromTopPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromTopPlaneOrientation (PlaneOrientation3d r f) =
  Orientation3d r f (Unit3d (r `cross` f))

-- | Construct an orientation from its bottom plane orientation.
fromBottomPlaneOrientation :: PlaneOrientation3d space -> Orientation3d space
fromBottomPlaneOrientation (PlaneOrientation3d l f) =
  Orientation3d (negative l) f (Unit3d (f `cross` l))

{-| Construct a backward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point backward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point upward.
-}
backwardOrientation :: Orientation3d space -> Orientation3d space
backwardOrientation = (.backwardOrientation)

{-| Construct a leftward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point leftward,
the rightward direction of the orientation will point forward,
and the upward direction of the orientation will point upward.
-}
leftwardOrientation :: Orientation3d space -> Orientation3d space
leftwardOrientation = (.leftwardOrientation)

{-| Construct a rightward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point rightward,
the rightward direction of the orientation will point backward,
and the upward direction of the orientation will point upward.
-}
rightwardOrientation :: Orientation3d space -> Orientation3d space
rightwardOrientation = (.rightwardOrientation)

{-| Construct an upward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point upward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point forward.
-}
upwardOrientation :: Orientation3d space -> Orientation3d space
upwardOrientation = (.upwardOrientation)

{-| Construct a downward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point downward,
the rightward direction of the orientation will point rightward,
and the upward direction of the orientation will point forward.
-}
downwardOrientation :: Orientation3d space -> Orientation3d space
downwardOrientation = (.downwardOrientation)

transformBy ::
  Transform3d.Rigid (space @ translationUnits) ->
  Orientation3d space ->
  Orientation3d space
transformBy transform (Orientation3d i j k) =
  Orientation3d
    (Direction3d.transformBy transform i)
    (Direction3d.transformBy transform j)
    (Direction3d.transformBy transform k)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  Orientation3d local ->
  Orientation3d global
placeIn globalFrame (Orientation3d i j k) =
  Orientation3d
    @ Direction3d.placeIn globalFrame i
    @ Direction3d.placeIn globalFrame j
    @ Direction3d.placeIn globalFrame k

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  Orientation3d global ->
  Orientation3d local
relativeTo globalFrame (Orientation3d i j k) =
  Orientation3d
    @ Direction3d.relativeTo globalFrame i
    @ Direction3d.relativeTo globalFrame j
    @ Direction3d.relativeTo globalFrame k

random :: Random.Generator (Orientation3d space)
random = Random.map fromTopPlaneOrientation PlaneOrientation3d.random
