module OpenSolid.Basis3d
  ( Basis3d
  , world
  , coerce
  , upwardDirection
  , downwardDirection
  , forwardDirection
  , backwardDirection
  , rightwardDirection
  , leftwardDirection
  , topPlaneBasis
  , bottomPlaneBasis
  , frontPlaneBasis
  , backPlaneBasis
  , rightPlaneBasis
  , leftPlaneBasis
  , forwardBasis
  , backwardBasis
  , leftwardBasis
  , rightwardBasis
  , upwardBasis
  , downwardBasis
  , transformBy
  , placeIn
  , relativeTo
  , inverse
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Basis3d (Basis3d)
  , Direction3d (Direction3d)
  , PlanarBasis3d (PlanarBasis3d)
  )
import OpenSolid.Transform3d qualified as Transform3d

{-| A basis having the global forward, backward, upward, downward, leftward and rightward directions.

Note that this will in general not be a true *global* basis
(i.e. one defining an earth-centered centered coordinate system),
but instead means roughly "the most global coordinate system known in the current context".
For example, if designing a particular bolt,
*eventually* that bolt will probably be placed in some other coordinate system
(one defining a train, or a building, or a piece of machinery that the bolt is used in)
but *when designing the bolt* the "most global" coordinate system known
is whatever coordinate system you have defined for the bolt itself.
-}
world :: Basis3d space (Defines space)
world =
  Basis3d
    # Direction3d 1.0 0.0 0.0
    # Direction3d 0.0 1.0 0.0
    # Direction3d 0.0 0.0 1.0

coerce :: Basis3d space1 defines1 -> Basis3d space2 defines2
coerce (Basis3d rightward forward upward) =
  Basis3d (Direction3d.coerce rightward) (Direction3d.coerce forward) (Direction3d.coerce upward)

-- | Get the rightward direction of a basis.
rightwardDirection :: Basis3d space defines -> Direction3d space
rightwardDirection (Basis3d r _ _) = r

-- | Get the forward direction of a basis.
forwardDirection :: Basis3d space defines -> Direction3d space
forwardDirection (Basis3d _ f _) = f

-- | Get the upward direction of a basis.
upwardDirection :: Basis3d space defines -> Direction3d space
upwardDirection (Basis3d _ _ u) = u

-- | Get the leftward direction of a basis.
leftwardDirection :: Basis3d space defines -> Direction3d space
leftwardDirection = negate . rightwardDirection

-- | Get the backward direction of a basis.
backwardDirection :: Basis3d space defines -> Direction3d space
backwardDirection = negate . forwardDirection

-- | Get the downward direction of a basis.
downwardDirection :: Basis3d space defines -> Direction3d space
downwardDirection = negate . upwardDirection

{-| Construct a forward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point forward,
the X direction of the planar basis will point leftward,
and the Y direction of the planar basis will point upward.
-}
frontPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
frontPlaneBasis basis = PlanarBasis3d (leftwardDirection basis) (upwardDirection basis)

{-| Construct a backward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point backward,
the X direction of the planar basis will point rightward,
and the Y direction of the planar basis will point upward.
-}
backPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
backPlaneBasis basis = PlanarBasis3d (rightwardDirection basis) (upwardDirection basis)

{-| Construct a leftward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point leftward,
the X direction of the planar basis will point backward,
and the Y direction of the planar basis will point upward.
-}
leftPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
leftPlaneBasis basis = PlanarBasis3d (backwardDirection basis) (upwardDirection basis)

{-| Construct a rightward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point rightward,
the X direction of the planar basis will point forward,
and the Y direction of the planar basis will point upward.
-}
rightPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
rightPlaneBasis basis = PlanarBasis3d (forwardDirection basis) (upwardDirection basis)

{-| Construct a upward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point upward,
the X direction of the planar basis will point rightward,
and the Y direction of the planar basis will point forward.
-}
topPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
topPlaneBasis basis = PlanarBasis3d (rightwardDirection basis) (forwardDirection basis)

{-| Construct a downward-facing planar basis from a parent basis.

Relative to the parent basis,
the normal direction of the planar basis will point downward,
the X direction of the planar basis will point leftward,
and the Y direction of the planar basis will point forward.
-}
bottomPlaneBasis :: Basis3d space defines1 -> PlanarBasis3d space defines2
bottomPlaneBasis basis = PlanarBasis3d (leftwardDirection basis) (forwardDirection basis)

{-| Construct a forward facing basis relative to a parent/reference basis.

This is a new basis with the same orientation as the parent basis.
-}
forwardBasis :: Basis3d space defines1 -> Basis3d space defines2
forwardBasis (Basis3d r f u) = Basis3d r f u

{-| Construct a backward facing basis relative to a parent/reference basis.

Relative to the parent basis,
the forward direction of the basis will point backward,
the rightward direction of the basis will point leftward,
and the upward direction of the basis will point upward.
-}
backwardBasis :: Basis3d space defines1 -> Basis3d space defines2
backwardBasis (Basis3d r f u) = Basis3d -r -f u

{-| Construct a leftward facing basis relative to a parent/reference basis.

Relative to the parent basis,
the forward direction of the basis will point leftward,
the rightward direction of the basis will point forward,
and the upward direction of the basis will point upward.
-}
leftwardBasis :: Basis3d space defines1 -> Basis3d space defines2
leftwardBasis (Basis3d r f u) = Basis3d f -r u

{-| Construct a rightward facing basis relative to a parent/reference basis.

Relative to the parent basis,
the forward direction of the basis will point rightward,
the rightward direction of the basis will point backward,
and the upward direction of the basis will point upward.
-}
rightwardBasis :: Basis3d space defines1 -> Basis3d space defines2
rightwardBasis (Basis3d r f u) = Basis3d -f r u

{-| Construct an upward facing basis relative to a parent/reference basis.

Relative to the parent basis,
the forward direction of the basis will point upward,
the rightward direction of the basis will point leftward,
and the upward direction of the basis will point forward.
-}
upwardBasis :: Basis3d space defines1 -> Basis3d space defines2
upwardBasis (Basis3d r f u) = Basis3d -r u f

{-| Construct a downward facing basis relative to a parent/reference basis.

Relative to the parent basis,
the forward direction of the basis will point downward,
the rightward direction of the basis will point rightward,
and the upward direction of the basis will point forward.
-}
downwardBasis :: Basis3d space defines1 -> Basis3d space defines2
downwardBasis (Basis3d r f u) = Basis3d r -u f

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
inverse basis = world |> relativeTo basis
