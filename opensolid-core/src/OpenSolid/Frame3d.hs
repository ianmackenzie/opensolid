module OpenSolid.Frame3d
  ( Frame3d (Frame3d)
  , coerce
  , erase
  , forward
  , backward
  , leftward
  , rightward
  , upward
  , downward
  , originPoint
  , orientation
  , rightwardDirection
  , leftwardDirection
  , forwardDirection
  , backwardDirection
  , upwardDirection
  , downwardDirection
  , rightwardAxis
  , leftwardAxis
  , forwardAxis
  , backwardAxis
  , upwardAxis
  , downwardAxis
  , frontPlane
  , backPlane
  , leftPlane
  , rightPlane
  , topPlane
  , bottomPlane
  , fromFrontPlane
  , fromBackPlane
  , fromLeftPlane
  , fromRightPlane
  , fromTopPlane
  , fromBottomPlane
  , offsetForwardBy
  , offsetBackwardBy
  , offsetRightwardBy
  , offsetLeftwardBy
  , offsetUpwardBy
  , offsetDownwardBy
  , turnRightBy
  , turnLeftBy
  , rollRightBy
  , rollLeftBy
  , tiltUpBy
  , tiltDownBy
  , turnRight
  , turnLeft
  , rollRight
  , rollLeft
  , tiltUp
  , tiltDown
  , placeIn
  , relativeTo
  , inverse
  , moveTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mate
  , align
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d (Frame3d), Plane3d (Plane3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.World3d qualified as World3d

-- | Get the origin point of a frame.
originPoint :: Frame3d space units defines -> Point3d space units
originPoint = (.originPoint)

-- | Get the orientation of a frame.
orientation :: Frame3d space units defines -> Orientation3d space
orientation = (.orientation)

coerce :: Frame3d space1 units1 defines1 -> Frame3d space2 units2 defines2
coerce (Frame3d p o) = Frame3d (Point3d.coerce p) (Orientation3d.coerce o)

erase :: Frame3d space units defines -> Frame3d space Unitless defines
erase = coerce

-- | Get the local rightward direction of a frame.
rightwardDirection :: Frame3d space units defines -> Direction3d space
rightwardDirection = (.rightwardDirection)

-- | Get the local leftward direction of a frame.
leftwardDirection :: Frame3d space units defines -> Direction3d space
leftwardDirection = (.leftwardDirection)

-- | Get the local forward direction of a frame.
forwardDirection :: Frame3d space units defines -> Direction3d space
forwardDirection = (.forwardDirection)

-- | Get the local backward direction of a frame.
backwardDirection :: Frame3d space units defines -> Direction3d space
backwardDirection = (.backwardDirection)

-- | Get the local upward direction of a frame.
upwardDirection :: Frame3d space units defines -> Direction3d space
upwardDirection = (.upwardDirection)

-- | Get the local downward direction of a frame.
downwardDirection :: Frame3d space units defines -> Direction3d space
downwardDirection = (.downwardDirection)

-- | Get the rightward axis of a frame.
rightwardAxis :: Frame3d space units defines -> Axis3d space units
rightwardAxis = (.rightwardAxis)

-- | Get the leftward axis of a frame.
leftwardAxis :: Frame3d space units defines -> Axis3d space units
leftwardAxis = (.leftwardAxis)

-- | Get the forward axis of a frame.
forwardAxis :: Frame3d space units defines -> Axis3d space units
forwardAxis = (.forwardAxis)

-- | Get the backward axis of a frame.
backwardAxis :: Frame3d space units defines -> Axis3d space units
backwardAxis = (.backwardAxis)

-- | Get the upward axis of a frame.
upwardAxis :: Frame3d space units defines -> Axis3d space units
upwardAxis = (.upwardAxis)

-- | Get the downward axis of a frame.
downwardAxis :: Frame3d space units defines -> Axis3d space units
downwardAxis = (.downwardAxis)

{-| Construct a locally forward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's forward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's upward direction.
-}
frontPlane :: Frame3d space units defines1 -> Plane3d space units defines2
frontPlane frame = Plane3d frame.originPoint frame.orientation.frontPlaneOrientation

{-| Construct a locally backward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's backward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's upward direction.
-}
backPlane :: Frame3d space units defines1 -> Plane3d space units defines2
backPlane frame = Plane3d frame.originPoint frame.orientation.backPlaneOrientation

{-| Construct a locally leftward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's leftward direction,
its X direction will be the frame's backward direction
and its Y direction will be frame's upward direction.
-}
leftPlane :: Frame3d space units defines1 -> Plane3d space units defines2
leftPlane frame = Plane3d frame.originPoint frame.orientation.leftPlaneOrientation

{-| Construct a locally rightward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's rightward direction,
its X direction will be the frame's forward direction
and its Y direction will be frame's upward direction.
-}
rightPlane :: Frame3d space units defines1 -> Plane3d space units defines2
rightPlane frame = Plane3d frame.originPoint frame.orientation.rightPlaneOrientation

{-| Construct a locally upward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's upward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's forward direction.
-}
topPlane :: Frame3d space units defines1 -> Plane3d space units defines2
topPlane frame = Plane3d frame.originPoint frame.orientation.topPlaneOrientation

{-| Construct a locally downward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's downward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's forward direction.
-}
bottomPlane :: Frame3d space units defines1 -> Plane3d space units defines2
bottomPlane frame = Plane3d frame.originPoint frame.orientation.bottomPlaneOrientation

-- | Construct a plane from its front plane.
fromFrontPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromFrontPlane (Plane3d p o) = Frame3d p (Orientation3d.fromFrontPlaneOrientation o)

-- | Construct a plane from its back plane.
fromBackPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromBackPlane (Plane3d p o) = Frame3d p (Orientation3d.fromBackPlaneOrientation o)

-- | Construct a plane from its left plane.
fromLeftPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromLeftPlane (Plane3d p o) = Frame3d p (Orientation3d.fromLeftPlaneOrientation o)

-- | Construct a plane from its right plane.
fromRightPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromRightPlane (Plane3d p o) = Frame3d p (Orientation3d.fromRightPlaneOrientation o)

-- | Construct a plane from its top plane.
fromTopPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromTopPlane (Plane3d p o) = Frame3d p (Orientation3d.fromTopPlaneOrientation o)

-- | Construct a plane from its bottom plane.
fromBottomPlane :: Plane3d space units defines1 -> Frame3d space units defines2
fromBottomPlane (Plane3d p o) = Frame3d p (Orientation3d.fromBottomPlaneOrientation o)

{-| Construct a forward-facing frame relative to a parent/reference frame.

This is just an identical copy of the parent frame,
but can be used to define a new/different local space.
-}
forward :: Frame3d space units defines1 -> Frame3d space units defines2
forward frame = Frame3d frame.originPoint frame.orientation

{-| Construct a backward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point backward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point leftward
(all relative to the parent frame).
-}
backward :: Frame3d space units defines1 -> Frame3d space units defines2
backward frame = Frame3d frame.originPoint frame.backwardOrientation

{-| Construct a leftward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point leftward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point forward
(all relative to the parent frame).
-}
leftward :: Frame3d space units defines1 -> Frame3d space units defines2
leftward frame = Frame3d frame.originPoint frame.leftwardOrientation

{-| Construct a rightward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point rightward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point backward
(all relative to the parent frame).
-}
rightward :: Frame3d space units defines1 -> Frame3d space units defines2
rightward frame = Frame3d frame.originPoint frame.rightwardOrientation

{-| Construct an upward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point upward,
the upward direction of the frame will point forward,
and the rightward direction of the frame will point leftward
(all relative to the parent frame).
-}
upward :: Frame3d space units defines1 -> Frame3d space units defines2
upward frame = Frame3d frame.originPoint frame.upwardOrientation

{-| Construct a downward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point downward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point rightward
(all relative to the parent frame).
-}
downward :: Frame3d space units defines1 -> Frame3d space units defines2
downward frame = Frame3d frame.originPoint frame.downwardOrientation

-- | Move a frame in its own forward direction by the given distance.
offsetForwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetForwardBy distance frame = translateIn (forwardDirection frame) distance frame

-- | Move a frame in its own backward direction by the given distance.
offsetBackwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetBackwardBy distance frame = translateIn (backwardDirection frame) distance frame

-- | Move a frame in its own rightward direction by the given distance.
offsetRightwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetRightwardBy distance frame = translateIn (rightwardDirection frame) distance frame

-- | Move a frame in its own leftward direction by the given distance.
offsetLeftwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetLeftwardBy distance frame = translateIn (leftwardDirection frame) distance frame

-- | Move a frame in its own upward direction by the given distance.
offsetUpwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetUpwardBy distance frame = translateIn (upwardDirection frame) distance frame

-- | Move a frame in its own downward direction by the given distance.
offsetDownwardBy ::
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
offsetDownwardBy distance frame = translateIn (downwardDirection frame) distance frame

{-| Rotate a frame counterclockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its leftward direction.
-}
turnLeftBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
turnLeftBy angle frame = rotateAround (upwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its rightward direction.
-}
turnRightBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
turnRightBy angle frame = rotateAround (upwardAxis frame) (negative angle) frame

{-| Rotate a frame counterclockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its rightward direction.
-}
rollRightBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
rollRightBy angle frame = rotateAround (forwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its leftward direction.
-}
rollLeftBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
rollLeftBy angle frame = rotateAround (forwardAxis frame) (negative angle) frame

{-| Rotate a frame counterclockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its upward direction.
-}
tiltUpBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
tiltUpBy angle frame = rotateAround (rightwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its downward direction.
-}
tiltDownBy :: Angle -> Frame3d space units defines1 -> Frame3d space units defines2
tiltDownBy angle frame = rotateAround (rightwardAxis frame) (negative angle) frame

{-| Turn a frame left by 90 degrees.

The forward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
turnLeft :: Frame3d space units defines1 -> Frame3d space units defines2
turnLeft = turnLeftBy Angle.halfPi

{-| Turn a frame right by 90 degrees.

The forward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
turnRight :: Frame3d space units defines1 -> Frame3d space units defines2
turnRight = turnRightBy Angle.halfPi

{-| Roll a frame left by 90 degrees.

The upward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
rollLeft :: Frame3d space units defines1 -> Frame3d space units defines2
rollLeft = rollLeftBy Angle.halfPi

{-| Roll a frame right by 90 degrees.

The upward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
rollRight :: Frame3d space units defines1 -> Frame3d space units defines2
rollRight = rollRightBy Angle.halfPi

{-| Tilt a frame up by 90 degrees.

The forward direction of the returned frame
will be equal to the upward direction of the original frame.
-}
tiltUp :: Frame3d space units defines1 -> Frame3d space units defines2
tiltUp = tiltUpBy Angle.halfPi

{-| Tilt a frame down by 90 degrees.

The forward direction of the returned frame
will be equal to the downward direction of the original frame.
-}
tiltDown :: Frame3d space units defines1 -> Frame3d space units defines2
tiltDown = tiltDownBy Angle.halfPi

-- | Convert a frame defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d global units (Defines local1) ->
  Frame3d local1 units (Defines local2) ->
  Frame3d global units (Defines local2)
placeIn globalFrame frame =
  Frame3d
    (Point3d.placeIn globalFrame (originPoint frame))
    (Orientation3d.placeIn globalFrame (orientation frame))

-- | Convert a frame defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d global units (Defines local1) ->
  Frame3d global units (Defines local2) ->
  Frame3d local1 units (Defines local2)
relativeTo globalFrame frame =
  Frame3d
    (Point3d.relativeTo globalFrame (originPoint frame))
    (Orientation3d.relativeTo globalFrame (orientation frame))

{-| Compute the "inverse" of a given frame.

This is a frame that defines the current global coordinate system
in terms of the frame's local coordinate system,
instead of the other way around.
-}
inverse :: Frame3d global units (Defines local) -> Frame3d local units (Defines global)
inverse frame = relativeTo frame World3d.frame

-- | Move a frame to a new origin point.
moveTo ::
  Point3d space units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
moveTo newOriginPoint (Frame3d _ o) = Frame3d newOriginPoint (Orientation3d.coerce o)

-- | Apply the given transform to a frame.
transformBy ::
  Transform3d.Rigid space units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
transformBy transform (Frame3d p o) =
  Frame3d (Point3d.transformBy transform p) (Orientation3d.transformBy transform o)

translateBy ::
  Vector3d space units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d space units ->
  Quantity units ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d space units ->
  Angle ->
  Frame3d space units defines1 ->
  Frame3d space units defines2
rotateAround = Transform3d.rotateAroundImpl transformBy

{-| Compute the relative orientation of two parent frames in order to align two child frames.

Imagine you have one object defined in coordinate system A ("local"),
with a particular frame X defined in coordinate system A.
Similarly, you have another object defined in coordinate system B ("global"),
with a particular frame Y defined in coordinate system B.
This function lets you determine the necessary relative alignment between A and B
such that the two frames X and Y are aligned with each other.
Given the two frames X and Y, this function returns a new frame
that defines the necessary orientation of A ("local") relative to B ("global").
-}
align ::
  Frame3d local units (Defines space) ->
  Frame3d global units (Defines space) ->
  Frame3d global units (Defines local)
align frame referenceFrame = placeIn referenceFrame (inverse frame)

{-| Compute the relative orientation of two parent frames in order to "mate" two child frames.

This is the same as 'align' except that the two child frames will end up facing towards each other
(with reversed forward directions, but the same upward directions)
instead of being aligned with each other.
-}
mate ::
  Frame3d local units (Defines space) ->
  Frame3d global units (Defines space) ->
  Frame3d global units (Defines local)
mate frame referenceFrame = align (backward frame) referenceFrame
