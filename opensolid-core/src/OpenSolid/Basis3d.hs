module OpenSolid.Basis3d
  ( Basis3d
  , coerce
  , identity
  , forwardFacing
  , backwardFacing
  , leftwardFacing
  , rightwardFacing
  , upwardFacing
  , downwardFacing
  , upwardDirection
  , downwardDirection
  , forwardDirection
  , backwardDirection
  , rightwardDirection
  , leftwardDirection
  , transformBy
  , placeIn
  , relativeTo
  , inverse
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives (Basis3d (..))
import OpenSolid.Transform3d qualified as Transform3d

coerce :: Basis3d space1 defines1 -> Basis3d space2 defines2
coerce (Basis3d i j k) =
  Basis3d (Direction3d.coerce i) (Direction3d.coerce j) (Direction3d.coerce k)

{-| A basis with the same orientation as the current coordinate system.

This is a basis whose forward direction points forward, upward direction points upward, etc.
-}
identity :: Basis3d space defines
identity = forwardFacing

{-| A forward facing basis.

The forward direction of the basis will point forward,
the rightward direction of the basis will point rightward,
and the upward direction of the basis will point upward.
-}
forwardFacing :: Basis3d space defines
forwardFacing =
  Basis3d
    { upwardDirection = Direction3d.upward
    , forwardDirection = Direction3d.forward
    , rightwardDirection = Direction3d.rightward
    }

{-| A backward facing basis.

The forward direction of the basis will point backward,
the rightward direction of the basis will point leftward,
and the upward direction of the basis will point upward.
-}
backwardFacing :: Basis3d space defines
backwardFacing =
  Basis3d
    { upwardDirection = Direction3d.upward
    , forwardDirection = Direction3d.backward
    , rightwardDirection = Direction3d.leftward
    }

{-| A leftward facing basis.

The forward direction of the basis will point leftward,
the rightward direction of the basis will point forward,
and the upward direction of the basis will point upward.
-}
leftwardFacing :: Basis3d space defines
leftwardFacing =
  Basis3d
    { upwardDirection = Direction3d.upward
    , forwardDirection = Direction3d.leftward
    , rightwardDirection = Direction3d.forward
    }

{-| A rightward facing basis.

The forward direction of the basis will point rightward,
the rightward direction of the basis will point backward,
and the upward direction of the basis will point upward.
-}
rightwardFacing :: Basis3d space defines
rightwardFacing =
  Basis3d
    { upwardDirection = Direction3d.upward
    , forwardDirection = Direction3d.rightward
    , rightwardDirection = Direction3d.backward
    }

{-| An upward facing basis.

The forward direction of the basis will point upward,
the rightward direction of the basis will point leftward,
and the upward direction of the basis will point forward.
-}
upwardFacing :: Basis3d space defines
upwardFacing =
  Basis3d
    { upwardDirection = Direction3d.forward
    , forwardDirection = Direction3d.upward
    , rightwardDirection = Direction3d.leftward
    }

{-| A downward facing basis.

The forward direction of the basis will point downward,
the rightward direction of the basis will point rightward,
and the upward direction of the basis will point forward.
-}
downwardFacing :: Basis3d space defines
downwardFacing =
  Basis3d
    { upwardDirection = Direction3d.forward
    , forwardDirection = Direction3d.downward
    , rightwardDirection = Direction3d.rightward
    }

-- | Get the leftward direction of a basis.
leftwardDirection :: Basis3d space defines -> Direction3d space
leftwardDirection = negate . rightwardDirection

-- | Get the backward direction of a basis.
backwardDirection :: Basis3d space defines -> Direction3d space
backwardDirection = negate . forwardDirection

-- | Get the downward direction of a basis.
downwardDirection :: Basis3d space defines -> Direction3d space
downwardDirection = negate . upwardDirection

transformBy ::
  Transform3d.Rigid (space @ translationUnits) ->
  Basis3d space defines1 ->
  Basis3d space defines2
transformBy transform (Basis3d i j k) =
  Basis3d
    (Direction3d.transformBy transform i)
    (Direction3d.transformBy transform j)
    (Direction3d.transformBy transform k)

-- | Convert a basis defined in local coordinates to one defined in global coordinates.
placeIn ::
  Basis3d global (Defines local) ->
  Basis3d local defines ->
  Basis3d global defines
placeIn globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.placeIn globalBasis i)
    (Direction3d.placeIn globalBasis j)
    (Direction3d.placeIn globalBasis k)

-- | Convert a basis defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Basis3d global (Defines local) ->
  Basis3d global defines ->
  Basis3d local defines
relativeTo globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.relativeTo globalBasis i)
    (Direction3d.relativeTo globalBasis j)
    (Direction3d.relativeTo globalBasis k)

inverse :: Basis3d global (Defines local) -> Basis3d local (Defines global)
inverse basis = forwardFacing |> relativeTo basis
