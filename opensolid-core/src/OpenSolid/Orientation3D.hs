module OpenSolid.Orientation3D
  ( Orientation3D
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

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3D (Unit3D)
  , Frame3D
  , Orientation3D (Orientation3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Transform3D qualified as Transform3D

coerce :: Orientation3D space1 -> Orientation3D space2
coerce (Orientation3D r f u) =
  Orientation3D (Direction3D.coerce r) (Direction3D.coerce f) (Direction3D.coerce u)

-- | Get the rightward direction of a orientation.
rightwardDirection :: Orientation3D space -> Direction3D space
rightwardDirection = (.rightwardDirection)

-- | Get the forward direction of a orientation.
forwardDirection :: Orientation3D space -> Direction3D space
forwardDirection = (.forwardDirection)

-- | Get the upward direction of a orientation.
upwardDirection :: Orientation3D space -> Direction3D space
upwardDirection = (.upwardDirection)

-- | Get the leftward direction of a orientation.
leftwardDirection :: Orientation3D space -> Direction3D space
leftwardDirection = (.leftwardDirection)

-- | Get the backward direction of a orientation.
backwardDirection :: Orientation3D space -> Direction3D space
backwardDirection = (.backwardDirection)

-- | Get the downward direction of a orientation.
downwardDirection :: Orientation3D space -> Direction3D space
downwardDirection = (.downwardDirection)

{-| Construct a forward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point forward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point upward.
-}
frontPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
frontPlaneOrientation = (.frontPlaneOrientation)

{-| Construct a backward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point backward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point upward.
-}
backPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
backPlaneOrientation = (.backPlaneOrientation)

{-| Construct a leftward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point leftward,
the X direction of the plane orientation will point backward,
and the Y direction of the plane orientation will point upward.
-}
leftPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
leftPlaneOrientation = (.leftPlaneOrientation)

{-| Construct a rightward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point rightward,
the X direction of the plane orientation will point forward,
and the Y direction of the plane orientation will point upward.
-}
rightPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
rightPlaneOrientation = (.rightPlaneOrientation)

{-| Construct a upward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point upward,
the X direction of the plane orientation will point rightward,
and the Y direction of the plane orientation will point forward.
-}
topPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
topPlaneOrientation = (.topPlaneOrientation)

{-| Construct a downward-facing plane orientation from a parent orientation.

Relative to the parent orientation,
the normal direction of the plane orientation will point downward,
the X direction of the plane orientation will point leftward,
and the Y direction of the plane orientation will point forward.
-}
bottomPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
bottomPlaneOrientation = (.bottomPlaneOrientation)

-- | Construct an orientation from its front plane orientation.
fromFrontPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromFrontPlaneOrientation (PlaneOrientation3D l u) =
  Orientation3D -l (Unit3D (l `cross` u)) u

-- | Construct an orientation from its back plane orientation.
fromBackPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromBackPlaneOrientation (PlaneOrientation3D r u) =
  Orientation3D r (Unit3D (u `cross` r)) u

-- | Construct an orientation from its left plane orientation.
fromLeftPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromLeftPlaneOrientation (PlaneOrientation3D b u) =
  Orientation3D (Unit3D (u `cross` b)) -b u

-- | Construct an orientation from its right plane orientation.
fromRightPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromRightPlaneOrientation (PlaneOrientation3D f u) =
  Orientation3D (Unit3D (f `cross` u)) f u

-- | Construct an orientation from its top plane orientation.
fromTopPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromTopPlaneOrientation (PlaneOrientation3D r f) =
  Orientation3D r f (Unit3D (r `cross` f))

-- | Construct an orientation from its bottom plane orientation.
fromBottomPlaneOrientation :: PlaneOrientation3D space -> Orientation3D space
fromBottomPlaneOrientation (PlaneOrientation3D l f) =
  Orientation3D -l f (Unit3D (f `cross` l))

{-| Construct a backward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point backward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point upward.
-}
backwardOrientation :: Orientation3D space -> Orientation3D space
backwardOrientation = (.backwardOrientation)

{-| Construct a leftward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point leftward,
the rightward direction of the orientation will point forward,
and the upward direction of the orientation will point upward.
-}
leftwardOrientation :: Orientation3D space -> Orientation3D space
leftwardOrientation = (.leftwardOrientation)

{-| Construct a rightward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point rightward,
the rightward direction of the orientation will point backward,
and the upward direction of the orientation will point upward.
-}
rightwardOrientation :: Orientation3D space -> Orientation3D space
rightwardOrientation = (.rightwardOrientation)

{-| Construct an upward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point upward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point forward.
-}
upwardOrientation :: Orientation3D space -> Orientation3D space
upwardOrientation = (.upwardOrientation)

{-| Construct a downward facing orientation relative to a parent/reference orientation.

Relative to the parent orientation,
the forward direction of the orientation will point downward,
the rightward direction of the orientation will point rightward,
and the upward direction of the orientation will point forward.
-}
downwardOrientation :: Orientation3D space -> Orientation3D space
downwardOrientation = (.downwardOrientation)

transformBy :: Transform3D.Rigid space -> Orientation3D space -> Orientation3D space
transformBy transform (Orientation3D i j k) =
  Orientation3D
    (Direction3D.transformBy transform i)
    (Direction3D.transformBy transform j)
    (Direction3D.transformBy transform k)

-- | Convert a orientation defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Orientation3D local -> Orientation3D global
placeIn globalFrame (Orientation3D i j k) =
  Orientation3D
    (Direction3D.placeIn globalFrame i)
    (Direction3D.placeIn globalFrame j)
    (Direction3D.placeIn globalFrame k)

-- | Convert a orientation defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Orientation3D global -> Orientation3D local
relativeTo globalFrame (Orientation3D i j k) =
  Orientation3D
    (Direction3D.relativeTo globalFrame i)
    (Direction3D.relativeTo globalFrame j)
    (Direction3D.relativeTo globalFrame k)

random :: Random.Generator (Orientation3D space)
random = Random.map fromTopPlaneOrientation PlaneOrientation3D.random
