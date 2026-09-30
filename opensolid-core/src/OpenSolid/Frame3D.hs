module OpenSolid.Frame3D
  ( Frame3D
  , with
  , coerce
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

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Orientation3D (Orientation3D)
import OpenSolid.Orientation3D qualified as Orientation3D
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.PlaneOrientation3D (PlaneOrientation3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3D (..), Plane3D (..))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.World3D qualified as World3D

with ::
  ( "originPoint" ::: Point3D global
  , "orientation" ::: Orientation3D global
  ) ->
  Frame3D global local
with params =
  recursive \self ->
    Frame3D
      { originPoint = params.originPoint
      , orientation = params.orientation
      , inverse =
          Frame3D
            { originPoint = Point3D.relativeTo self World3D.originPoint
            , orientation = Orientation3D.relativeTo self World3D.forwardOrientation
            , inverse = self
            }
      }

-- | Get the origin point of a frame.
originPoint :: Frame3D global local -> Point3D global
originPoint = (.originPoint)

-- | Get the orientation of a frame.
orientation :: Frame3D global local -> Orientation3D global
orientation = (.orientation)

{-# INLINE coerce #-}
coerce :: Frame3D global1 local1 -> Frame3D global2 local2
coerce = Data.Coerce.coerce

-- | Get the local rightward direction of a frame.
rightwardDirection :: Frame3D global local -> Direction3D global
rightwardDirection frame = Orientation3D.rightwardDirection (orientation frame)

-- | Get the local leftward direction of a frame.
leftwardDirection :: Frame3D global local -> Direction3D global
leftwardDirection frame = Orientation3D.leftwardDirection (orientation frame)

-- | Get the local forward direction of a frame.
forwardDirection :: Frame3D global local -> Direction3D global
forwardDirection frame = Orientation3D.forwardDirection (orientation frame)

-- | Get the local backward direction of a frame.
backwardDirection :: Frame3D global local -> Direction3D global
backwardDirection frame = Orientation3D.backwardDirection (orientation frame)

-- | Get the local upward direction of a frame.
upwardDirection :: Frame3D global local -> Direction3D global
upwardDirection frame = Orientation3D.upwardDirection (orientation frame)

-- | Get the local downward direction of a frame.
downwardDirection :: Frame3D global local -> Direction3D global
downwardDirection frame = Orientation3D.downwardDirection (orientation frame)

axisIn :: (Frame3D global local -> Direction3D global) -> Frame3D global local -> Axis3D global
axisIn direction frame = Axis3D.with {originPoint = originPoint frame, direction = direction frame}

-- | Get the rightward axis of a frame.
rightwardAxis :: Frame3D global local -> Axis3D global
rightwardAxis = axisIn rightwardDirection

-- | Get the leftward axis of a frame.
leftwardAxis :: Frame3D global local -> Axis3D global
leftwardAxis = axisIn leftwardDirection

-- | Get the forward axis of a frame.
forwardAxis :: Frame3D global local -> Axis3D global
forwardAxis = axisIn forwardDirection

-- | Get the backward axis of a frame.
backwardAxis :: Frame3D global local -> Axis3D global
backwardAxis = axisIn backwardDirection

-- | Get the upward axis of a frame.
upwardAxis :: Frame3D global local -> Axis3D global
upwardAxis = axisIn upwardDirection

-- | Get the downward axis of a frame.
downwardAxis :: Frame3D global local -> Axis3D global
downwardAxis = axisIn downwardDirection

planeWith ::
  (Orientation3D global -> PlaneOrientation3D global) ->
  Frame3D global local ->
  Plane3D global
planeWith planeOrientation frame =
  Plane3D
    { originPoint = originPoint frame
    , orientation = planeOrientation (orientation frame)
    }

{-| Construct a locally forward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's forward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's upward direction.
-}
frontPlane :: Frame3D global local -> Plane3D global
frontPlane = planeWith Orientation3D.frontPlaneOrientation

{-| Construct a locally backward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's backward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's upward direction.
-}
backPlane :: Frame3D global local -> Plane3D global
backPlane = planeWith Orientation3D.backPlaneOrientation

{-| Construct a locally leftward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's leftward direction,
its X direction will be the frame's backward direction
and its Y direction will be frame's upward direction.
-}
leftPlane :: Frame3D global local -> Plane3D global
leftPlane = planeWith Orientation3D.leftPlaneOrientation

{-| Construct a locally rightward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's rightward direction,
its X direction will be the frame's forward direction
and its Y direction will be frame's upward direction.
-}
rightPlane :: Frame3D global local -> Plane3D global
rightPlane = planeWith Orientation3D.rightPlaneOrientation

{-| Construct a locally upward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's upward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's forward direction.
-}
topPlane :: Frame3D global local -> Plane3D global
topPlane = planeWith Orientation3D.topPlaneOrientation

{-| Construct a locally downward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's downward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's forward direction.
-}
bottomPlane :: Frame3D global local -> Plane3D global
bottomPlane = planeWith Orientation3D.bottomPlaneOrientation

fromPlane ::
  (PlaneOrientation3D global -> Orientation3D global) ->
  Plane3D global ->
  Frame3D global local
fromPlane fromPlaneOrientation plane =
  with
    { originPoint = Plane3D.originPoint plane
    , orientation = fromPlaneOrientation (Plane3D.orientation plane)
    }

-- | Construct a plane from its front plane.
fromFrontPlane :: Plane3D global -> Frame3D global local
fromFrontPlane = fromPlane Orientation3D.fromFrontPlaneOrientation

-- | Construct a plane from its back plane.
fromBackPlane :: Plane3D global -> Frame3D global local
fromBackPlane = fromPlane Orientation3D.fromBackPlaneOrientation

-- | Construct a plane from its left plane.
fromLeftPlane :: Plane3D global -> Frame3D global local
fromLeftPlane = fromPlane Orientation3D.fromLeftPlaneOrientation

-- | Construct a plane from its right plane.
fromRightPlane :: Plane3D global -> Frame3D global local
fromRightPlane = fromPlane Orientation3D.fromRightPlaneOrientation

-- | Construct a plane from its top plane.
fromTopPlane :: Plane3D global -> Frame3D global local
fromTopPlane = fromPlane Orientation3D.fromTopPlaneOrientation

-- | Construct a plane from its bottom plane.
fromBottomPlane :: Plane3D global -> Frame3D global local
fromBottomPlane = fromPlane Orientation3D.fromBottomPlaneOrientation

{-| Construct a forward-facing frame relative to a parent/reference frame.

This is just an identical copy of the parent frame,
but can be used to define a new/different local space.
-}
forward :: Frame3D global local1 -> Frame3D global local2
forward frame = with {originPoint = frame.originPoint, orientation = frame.orientation}

reorient ::
  (Orientation3D global -> Orientation3D global) ->
  Frame3D global local1 ->
  Frame3D global local2
reorient orientationFunction frame =
  with
    { originPoint = originPoint frame
    , orientation = orientationFunction (orientation frame)
    }

{-| Construct a backward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point backward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point leftward
(all relative to the parent frame).
-}
backward :: Frame3D global local1 -> Frame3D global local2
backward = reorient Orientation3D.backwardOrientation

{-| Construct a leftward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point leftward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point forward
(all relative to the parent frame).
-}
leftward :: Frame3D global local1 -> Frame3D global local2
leftward = reorient Orientation3D.leftwardOrientation

{-| Construct a rightward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point rightward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point backward
(all relative to the parent frame).
-}
rightward :: Frame3D global local1 -> Frame3D global local2
rightward = reorient Orientation3D.rightwardOrientation

{-| Construct an upward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point upward,
the upward direction of the frame will point forward,
and the rightward direction of the frame will point leftward
(all relative to the parent frame).
-}
upward :: Frame3D global local1 -> Frame3D global local2
upward = reorient Orientation3D.upwardOrientation

{-| Construct a downward-facing frame relative to a parent/reference frame.

The forward direction of the frame will point downward,
the upward direction of the frame will point forward,
and the rightward direction of the frame will point rightward
(all relative to the parent frame).
-}
downward :: Frame3D global local1 -> Frame3D global local2
downward = reorient Orientation3D.downwardOrientation

-- | Move a frame in its own forward direction by the given distance.
offsetForwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetForwardBy distance frame = translateIn (forwardDirection frame) distance frame

-- | Move a frame in its own backward direction by the given distance.
offsetBackwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetBackwardBy distance frame = translateIn (backwardDirection frame) distance frame

-- | Move a frame in its own rightward direction by the given distance.
offsetRightwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetRightwardBy distance frame = translateIn (rightwardDirection frame) distance frame

-- | Move a frame in its own leftward direction by the given distance.
offsetLeftwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetLeftwardBy distance frame = translateIn (leftwardDirection frame) distance frame

-- | Move a frame in its own upward direction by the given distance.
offsetUpwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetUpwardBy distance frame = translateIn (upwardDirection frame) distance frame

-- | Move a frame in its own downward direction by the given distance.
offsetDownwardBy :: Length -> Frame3D global local1 -> Frame3D global local2
offsetDownwardBy distance frame = translateIn (downwardDirection frame) distance frame

{-| Rotate a frame counterclockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its leftward direction.
-}
turnLeftBy :: Angle -> Frame3D global local1 -> Frame3D global local2
turnLeftBy angle frame = rotateAround (upwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its rightward direction.
-}
turnRightBy :: Angle -> Frame3D global local1 -> Frame3D global local2
turnRightBy angle frame = rotateAround (upwardAxis frame) -angle frame

{-| Rotate a frame counterclockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its rightward direction.
-}
rollRightBy :: Angle -> Frame3D global local1 -> Frame3D global local2
rollRightBy angle frame = rotateAround (forwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its leftward direction.
-}
rollLeftBy :: Angle -> Frame3D global local1 -> Frame3D global local2
rollLeftBy angle frame = rotateAround (forwardAxis frame) -angle frame

{-| Rotate a frame counterclockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its upward direction.
-}
tiltUpBy :: Angle -> Frame3D global local1 -> Frame3D global local2
tiltUpBy angle frame = rotateAround (rightwardAxis frame) angle frame

{-| Rotate a frame clockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its downward direction.
-}
tiltDownBy :: Angle -> Frame3D global local1 -> Frame3D global local2
tiltDownBy angle frame = rotateAround (rightwardAxis frame) -angle frame

{-| Turn a frame left by 90 degrees.

The forward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
turnLeft :: Frame3D global local1 -> Frame3D global local2
turnLeft = turnLeftBy Angle.halfPi

{-| Turn a frame right by 90 degrees.

The forward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
turnRight :: Frame3D global local1 -> Frame3D global local2
turnRight = turnRightBy Angle.halfPi

{-| Roll a frame left by 90 degrees.

The upward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
rollLeft :: Frame3D global local1 -> Frame3D global local2
rollLeft = rollLeftBy Angle.halfPi

{-| Roll a frame right by 90 degrees.

The upward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
rollRight :: Frame3D global local1 -> Frame3D global local2
rollRight = rollRightBy Angle.halfPi

{-| Tilt a frame up by 90 degrees.

The forward direction of the returned frame
will be equal to the upward direction of the original frame.
-}
tiltUp :: Frame3D global local1 -> Frame3D global local2
tiltUp = tiltUpBy Angle.halfPi

{-| Tilt a frame down by 90 degrees.

The forward direction of the returned frame
will be equal to the downward direction of the original frame.
-}
tiltDown :: Frame3D global local1 -> Frame3D global local2
tiltDown = tiltDownBy Angle.halfPi

-- | Convert a frame defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D space1 space2 -> Frame3D space2 space3 -> Frame3D space1 space3
placeIn globalFrame frame =
  with
    { originPoint = Point3D.placeIn globalFrame (originPoint frame)
    , orientation = Orientation3D.placeIn globalFrame (orientation frame)
    }

-- | Convert a frame defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D space1 space2 -> Frame3D space1 space3 -> Frame3D space2 space3
relativeTo globalFrame frame =
  with
    { originPoint = Point3D.relativeTo globalFrame (originPoint frame)
    , orientation = Orientation3D.relativeTo globalFrame (orientation frame)
    }

{-| Get the "inverse" of a given frame.

This is a frame that defines the current global coordinate system
in terms of the frame's local coordinate system,
instead of the other way around.
-}
inverse :: Frame3D global local -> Frame3D local global
inverse = (.inverse)

-- | Move a frame to a new origin point.
moveTo :: Point3D global -> Frame3D global local1 -> Frame3D global local2
moveTo newOriginPoint frame =
  with
    { originPoint = newOriginPoint
    , orientation = orientation frame
    }

-- | Apply the given transform to a frame.
transformBy :: Transform3D.Rigid global -> Frame3D global local1 -> Frame3D global local2
transformBy transform frame =
  with
    { originPoint = originPoint frame & Point3D.transformBy transform
    , orientation =
        orientation frame
          & Orientation3D.transformBy (Transform3D.vectorTransform transform)
    }

translateBy :: Vector3D Meters global -> Frame3D global local1 -> Frame3D global local2
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D global -> Length -> Frame3D global local1 -> Frame3D global local2
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D global -> Length -> Frame3D global local1 -> Frame3D global local2
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D global -> Angle -> Frame3D global local1 -> Frame3D global local2
rotateAround = Transform3D.rotateAroundImpl transformBy

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
align :: Frame3D space1 space3 -> Frame3D space2 space3 -> Frame3D space2 space1
align frame referenceFrame = placeIn referenceFrame (inverse frame)

{-| Compute the relative orientation of two parent frames in order to "mate" two child frames.

This is the same as 'align' except that the two child frames will end up facing towards each other
(with reversed forward directions, but the same upward directions)
instead of being aligned with each other.
-}
mate :: Frame3D space1 space3 -> Frame3D space2 space3 -> Frame3D space2 space1
mate frame referenceFrame = align (backward frame) referenceFrame
