module OpenSolid.World3D
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
  , forwardPlane
  , backwardPlane
  , rightwardPlane
  , leftwardPlane
  , upwardPlane
  , downwardPlane
  , forwardPlaneOrientation
  , backwardPlaneOrientation
  , rightwardPlaneOrientation
  , leftwardPlaneOrientation
  , upwardPlaneOrientation
  , downwardPlaneOrientation
  , forwardAxis
  , backwardAxis
  , leftwardAxis
  , rightwardAxis
  , upwardAxis
  , downwardAxis
  )
where

import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D)
  , Frame3D (Frame3D)
  , Orientation3D (Orientation3D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point3D (Point3D)
  )

-- | The global origin point of a coordinate system.
originPoint :: Point3D space
originPoint = Point3D Length.zero Length.zero Length.zero

-- | The global rightward direction of a coordinate system.
rightwardDirection :: Direction3D space
rightwardDirection = Direction3D 1.0 0.0 0.0

-- | The global leftward direction of a coordinate system.
leftwardDirection :: Direction3D space
leftwardDirection = -rightwardDirection

-- | The global forward direction of a coordinate system.
forwardDirection :: Direction3D space
forwardDirection = Direction3D 0.0 1.0 0.0

-- | The global backward direction of a coordinate system.
backwardDirection :: Direction3D space
backwardDirection = -forwardDirection

-- | The global upward direction of a coordinate system.
upwardDirection :: Direction3D space
upwardDirection = Direction3D 0.0 0.0 1.0

-- | The global downward direction of a coordinate system.
downwardDirection :: Direction3D space
downwardDirection = -upwardDirection

{-| The forward-facing or 'default' orientation of the global coordinate system.

The directions of this orientation will match the global directions:
the forward direction will point forward,
the rightward direction will point rightward,
and the upward direction will point upward.
-}
forwardOrientation :: Orientation3D space
forwardOrientation = Orientation3D rightwardDirection forwardDirection upwardDirection

{-| The backward-facing orientation of the global coordinate system.

The forward direction of the orientation will point backward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point upward.
-}
backwardOrientation :: Orientation3D space
backwardOrientation = Orientation3D leftwardDirection backwardDirection upwardDirection

{-| The leftward-facing orientation of the global coordinate system.

The forward direction of the orientation will point leftward,
the rightward direction of the orientation will point forward,
and the upward direction of the orientation will point upward.
-}
leftwardOrientation :: Orientation3D space
leftwardOrientation = Orientation3D forwardDirection leftwardDirection upwardDirection

{-| The rightward-facing orientation of the global coordinate system.

The forward direction of the orientation will point rightward,
the rightward direction of the orientation will point backward,
and the upward direction of the orientation will point upward.
-}
rightwardOrientation :: Orientation3D space
rightwardOrientation = Orientation3D backwardDirection rightwardDirection upwardDirection

{-| The upward-facing orientation of the global coordinate system.

The forward direction of the orientation will point upward,
the rightward direction of the orientation will point leftward,
and the upward direction of the orientation will point forward.
-}
upwardOrientation :: Orientation3D space
upwardOrientation = Orientation3D leftwardDirection upwardDirection forwardDirection

{-| The downward-facing orientation of the global coordinate system.

The forward direction of the orientation will point downward,
the rightward direction of the orientation will point rightward,
and the upward direction of the orientation will point forward.
-}
downwardOrientation :: Orientation3D space
downwardOrientation = Orientation3D rightwardDirection downwardDirection forwardDirection

-- | A frame of reference defining a global coordinate system.
frame :: Frame3D space space
frame = Frame3D originPoint forwardOrientation

{-| A forward-facing plane through the global origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global upward direction.
-}
frontPlane :: Plane3D space
frontPlane = forwardPlane originPoint

{-| A forward-facing plane through the given origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global upward direction.
-}
forwardPlane :: Point3D space -> Plane3D space
forwardPlane givenOriginPoint = Plane3D givenOriginPoint forwardPlaneOrientation

{-| The orientation of a global forward-facing plane.

The X direction of the orientation will be the global leftward direction,
and the Y direction of the orientation will be the global upward direction.
-}
forwardPlaneOrientation :: PlaneOrientation3D space
forwardPlaneOrientation = PlaneOrientation3D leftwardDirection upwardDirection

{-| A backward-facing plane through the global origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global upward direction.
-}
backPlane :: Plane3D space
backPlane = backwardPlane originPoint

{-| A backward-facing plane through the given origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global upward direction.
-}
backwardPlane :: Point3D space -> Plane3D space
backwardPlane givenOriginPoint = Plane3D givenOriginPoint backwardPlaneOrientation

{-| The orientation of a global backward-facing plane.

The X direction of the orientation will be the global rightward direction,
and the Y direction of the orientation will be the global upward direction.
-}
backwardPlaneOrientation :: PlaneOrientation3D space
backwardPlaneOrientation = PlaneOrientation3D rightwardDirection upwardDirection

{-| An upward-facing plane through the global origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global forward direction.
-}
topPlane :: Plane3D space
topPlane = upwardPlane originPoint

{-| An upward-facing plane through the given origin point.

The X direction of the plane will be the global rightward direction,
and the Y direction of the plane will be the global forward direction.
-}
upwardPlane :: Point3D space -> Plane3D space
upwardPlane givenOriginPoint = Plane3D givenOriginPoint upwardPlaneOrientation

{-| The orientation of a global upward-facing plane.

The X direction of the orientation will be the global rightward direction,
and the Y direction of the orientation will be the global forward direction.
-}
upwardPlaneOrientation :: PlaneOrientation3D space
upwardPlaneOrientation = PlaneOrientation3D rightwardDirection forwardDirection

{-| A downward-facing plane through the global origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global forward direction.
-}
bottomPlane :: Plane3D space
bottomPlane = downwardPlane originPoint

{-| A downward-facing plane through the given origin point.

The X direction of the plane will be the global leftward direction,
and the Y direction of the plane will be the global forward direction.
-}
downwardPlane :: Point3D space -> Plane3D space
downwardPlane givenOriginPoint = Plane3D givenOriginPoint downwardPlaneOrientation

{-| The orientation of a global downward-facing plane.

The X direction of the orientation will be the global leftward direction,
and the Y direction of the orientation will be the global forward direction.
-}
downwardPlaneOrientation :: PlaneOrientation3D space
downwardPlaneOrientation = PlaneOrientation3D leftwardDirection forwardDirection

{-| A rightward-facing plane through the global origin point.

The X direction of the plane will be the global forward direction,
and the Y direction of the plane will be the global upward direction.
-}
rightPlane :: Plane3D space
rightPlane = rightwardPlane originPoint

{-| A rightward-facing plane through the given origin point.

The X direction of the plane will be the global forward direction,
and the Y direction of the plane will be the global upward direction.
-}
rightwardPlane :: Point3D space -> Plane3D space
rightwardPlane givenOriginPoint = Plane3D givenOriginPoint rightwardPlaneOrientation

{-| The orientation of a global rightward-facing plane.

The X direction of the orientation will be the global forward direction,
and the Y direction of the orientation will be the global upward direction.
-}
rightwardPlaneOrientation :: PlaneOrientation3D space
rightwardPlaneOrientation = PlaneOrientation3D forwardDirection upwardDirection

{-| A leftward-facing plane through the global origin point.

The X direction of the plane will be the global backward direction,
and the Y direction of the plane will be the global upward direction.
-}
leftPlane :: Plane3D space
leftPlane = leftwardPlane originPoint

{-| A leftward-facing plane through the given origin point.

The X direction of the plane will be the global backward direction,
and the Y direction of the plane will be the global upward direction.
-}
leftwardPlane :: Point3D space -> Plane3D space
leftwardPlane givenOriginPoint = Plane3D givenOriginPoint leftwardPlaneOrientation

{-| The orientation of a global leftward-facing plane.

The X direction of the orientation will be the global backward direction,
and the Y direction of the orientation will be the global upward direction.
-}
leftwardPlaneOrientation :: PlaneOrientation3D space
leftwardPlaneOrientation = PlaneOrientation3D backwardDirection upwardDirection

-- | A forward-facing axis through the global origin point.
forwardAxis :: Axis3D space
forwardAxis = Axis3D originPoint forwardDirection

-- | A backward-facing axis through the global origin point.
backwardAxis :: Axis3D space
backwardAxis = Axis3D originPoint backwardDirection

-- | An upward-facing axis through the global origin point.
upwardAxis :: Axis3D space
upwardAxis = Axis3D originPoint upwardDirection

-- | A downward-facing axis through the global origin point.
downwardAxis :: Axis3D space
downwardAxis = Axis3D originPoint downwardDirection

-- | A leftward-facing axis through the global origin point.
leftwardAxis :: Axis3D space
leftwardAxis = Axis3D originPoint leftwardDirection

-- | A rightward-facing axis through the global origin point.
rightwardAxis :: Axis3D space
rightwardAxis = Axis3D originPoint rightwardDirection
