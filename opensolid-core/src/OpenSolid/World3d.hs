module OpenSolid.World3d
  ( originPoint
  , forwardDirection
  , backwardDirection
  , leftwardDirection
  , rightwardDirection
  , upwardDirection
  , downwardDirection
  , forwardOrientation
  , backwardOrientation
  , upwardOrientation
  , downwardOrientation
  , rightwardOrientation
  , leftwardOrientation
  , frame
  , frontPlane
  , backPlane
  , rightPlane
  , leftPlane
  , topPlane
  , bottomPlane
  , forwardAxis
  , backwardAxis
  , leftwardAxis
  , rightwardAxis
  , upwardAxis
  , downwardAxis
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d)
  )
import OpenSolid.Quantity qualified as Quantity

-- | The global origin point of a coordinate system.
originPoint :: Point3d (space @ units)
originPoint = Point3d Quantity.zero Quantity.zero Quantity.zero

-- | The global rightward direction of a coordinate system.
rightwardDirection :: Direction3d space
rightwardDirection = Direction3d 1 0 0

-- | The global leftward direction of a coordinate system.
leftwardDirection :: Direction3d space
leftwardDirection = negative rightwardDirection

-- | The global forward direction of a coordinate system.
forwardDirection :: Direction3d space
forwardDirection = Direction3d 0 1 0

-- | The global backward direction of a coordinate system.
backwardDirection :: Direction3d space
backwardDirection = negative forwardDirection

-- | The global upward direction of a coordinate system.
upwardDirection :: Direction3d space
upwardDirection = Direction3d 0 0 1

-- | The global downward direction of a coordinate system.
downwardDirection :: Direction3d space
downwardDirection = negative upwardDirection

{-| The forward-facing or 'default' orientation of the global coordinate system.

The directions of this orientation will match the global directions:
the forward direction will point forward,
the rightward direction will point rightward,
and the upward direction will point upward.
-}
forwardOrientation :: Orientation3d space
forwardOrientation = Orientation3d rightwardDirection forwardDirection upwardDirection

{-| The backward-facing orientation of the global coordinate system.

The forward direction of the orientation will point backward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point upward.
-}
backwardOrientation :: Orientation3d space
backwardOrientation = Orientation3d leftwardDirection backwardDirection upwardDirection

{-| The leftward-facing orientation of the global coordinate system.

The forward direction of the orientation will point leftward,
the rightward direction of the orientation will point forward,
and the upward direction of the orientation will point upward.
-}
leftwardOrientation :: Orientation3d space
leftwardOrientation = Orientation3d forwardDirection leftwardDirection upwardDirection

{-| The rightward-facing orientation of the global coordinate system.

The forward direction of the orientation will point rightward,
the rightward direction of the orientation will point backward,
and the upward direction of the orientation will point upward.
-}
rightwardOrientation :: Orientation3d space
rightwardOrientation = Orientation3d backwardDirection rightwardDirection upwardDirection

{-| The upward-facing orientation of the global coordinate system.

The forward direction of the orientation will point upward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point forward.
-}
upwardOrientation :: Orientation3d space
upwardOrientation = Orientation3d leftwardDirection upwardDirection forwardDirection

{-| The downward-facing orientation of the global coordinate system.

The forward direction of the orientation will point downward,
the rightward direction of the orientation will point rightward,
and the upward direction of the orientation will point forward.
-}
downwardOrientation :: Orientation3d space
downwardOrientation = Orientation3d rightwardDirection downwardDirection forwardDirection

-- | A frame of reference defining a global coordinate system.
frame :: Frame3d (space @ units) (Defines space)
frame = Frame3d originPoint forwardOrientation

{-| A forward-facing plane through the global origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global upward direction.
-}
frontPlane :: Plane3d (space @ units) defines
frontPlane = Plane3d originPoint (PlaneOrientation3d leftwardDirection upwardDirection)

{-| A backward-facing plane through the global origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global upward direction.
-}
backPlane :: Plane3d (space @ units) defines
backPlane = Plane3d originPoint (PlaneOrientation3d rightwardDirection upwardDirection)

{-| An upward-facing plane through the global origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global forward direction.
-}
topPlane :: Plane3d (space @ units) defines
topPlane = Plane3d originPoint (PlaneOrientation3d rightwardDirection forwardDirection)

{-| An downward-facing plane through the global origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global forward direction.
-}
bottomPlane :: Plane3d (space @ units) defines
bottomPlane = Plane3d originPoint (PlaneOrientation3d leftwardDirection forwardDirection)

{-| A rightward-facing plane through the global origin point.

The X direction of the plane will be the global forward direction,
and the Y direction of the plane will be the global upward direction.
-}
rightPlane :: Plane3d (space @ units) defines
rightPlane = Plane3d originPoint (PlaneOrientation3d forwardDirection upwardDirection)

{-| A leftward-facing plane through the global origin point.

The X direction of the plane will be the global backward direction,
and the Y direction of the plane will be the global upward direction.
-}
leftPlane :: Plane3d (space @ units) defines
leftPlane = Plane3d originPoint (PlaneOrientation3d backwardDirection upwardDirection)

-- | A forward-facing axis through the global origin point.
forwardAxis :: Axis3d (space @ units)
forwardAxis = Axis3d originPoint forwardDirection

-- | A backward-facing axis through the global origin point.
backwardAxis :: Axis3d (space @ units)
backwardAxis = Axis3d originPoint backwardDirection

-- | An upward-facing axis through the global origin point.
upwardAxis :: Axis3d (space @ units)
upwardAxis = Axis3d originPoint upwardDirection

-- | A downward-facing axis through the global origin point.
downwardAxis :: Axis3d (space @ units)
downwardAxis = Axis3d originPoint downwardDirection

-- | A leftward-facing axis through the global origin point.
leftwardAxis :: Axis3d (space @ units)
leftwardAxis = Axis3d originPoint leftwardDirection

-- | A rightward-facing axis through the global origin point.
rightwardAxis :: Axis3d (space @ units)
rightwardAxis = Axis3d originPoint rightwardDirection
