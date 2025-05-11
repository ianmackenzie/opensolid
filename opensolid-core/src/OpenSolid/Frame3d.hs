module OpenSolid.Frame3d
  ( Frame3d (Frame3d)
  , coerce
  , erase
  , identity
  , forwardFacing
  , backwardFacing
  , leftwardFacing
  , rightwardFacing
  , upwardFacing
  , downwardFacing
  , originPoint
  , basis
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
  , reverse
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
  , rotateUpBy
  , rotateDownBy
  , turnRight
  , turnLeft
  , rollRight
  , rollLeft
  , rotateUp
  , rotateDown
  , placeIn
  , relativeTo
  , inverse
  , moveTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  , mate
  , align
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3d (Axis3d (Axis3d))
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Basis3d (Basis3d)
  , Direction3d (Unit3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Vector3d
  )
import OpenSolid.Transform3d qualified as Transform3d

{-| A frame of reference at the origin point of the current coordinate system, facing forward.

This effectively *is* the current coordinate system, i.e.
point coordinates relative to this frame
will be exactly their coordinates in the current coordinate system.
-}
identity :: Frame3d (space @ units) defines
identity = forwardFacing Point3d.origin

{-| Construct a forward-facing frame at the given point.

The orientation of the frame will be the same as the current coordinate system
(the forward direction of the frame will point forward, etc.)
-}
forwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
forwardFacing p0 = Frame3d p0 Basis3d.forwardFacing

{-| Construct a backward-facing frame at the given point.

The forward direction of the frame will point backward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point leftward.
-}
backwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
backwardFacing p0 = Frame3d p0 Basis3d.backwardFacing

{-| Construct a leftward-facing frame at the given point.

The forward direction of the frame will point leftward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point forward.
-}
leftwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
leftwardFacing p0 = Frame3d p0 Basis3d.leftwardFacing

{-| Construct a rightward-facing frame at the given point.

The forward direction of the frame will point rightward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point backward.
-}
rightwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
rightwardFacing p0 = Frame3d p0 Basis3d.rightwardFacing

{-| Construct an upward-facing frame at the given point.

The forward direction of the frame will point upward,
the upward direction of the frame will point forward,
and the rightward direction of the frame will point leftward.
-}
upwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
upwardFacing p0 = Frame3d p0 Basis3d.upwardFacing

{-| Construct a downward-facing frame at the given point.

The forward direction of the frame will point downward,
the upward direction of the frame will point upward,
and the rightward direction of the frame will point rightward.
-}
downwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
downwardFacing p0 = Frame3d p0 Basis3d.downwardFacing

-- | Get the origin point of a frame.
originPoint :: Frame3d (space @ units) defines -> Point3d (space @ units)
originPoint (Frame3d p0 _) = p0

-- | Get the basis (orientation) of a frame.
basis :: Frame3d (space @ units) defines -> Basis3d space defines
basis (Frame3d _ b) = b

coerce :: Frame3d (space1 @ units1) defines1 -> Frame3d (space2 @ units2) defines2
coerce (Frame3d p0 b) = Frame3d (Point3d.coerce p0) (Basis3d.coerce b)

erase :: Frame3d (space @ units) defines -> Frame3d (space @ Unitless) defines
erase = coerce

-- | Get the local rightward direction of a frame.
rightwardDirection :: Frame3d (space @ units) defines -> Direction3d space
rightwardDirection frame = Basis3d.rightwardDirection (basis frame)

-- | Get the local leftward direction of a frame.
leftwardDirection :: Frame3d (space @ units) defines -> Direction3d space
leftwardDirection frame = Basis3d.leftwardDirection (basis frame)

-- | Get the local forward direction of a frame.
forwardDirection :: Frame3d (space @ units) defines -> Direction3d space
forwardDirection frame = Basis3d.forwardDirection (basis frame)

-- | Get the local backward direction of a frame.
backwardDirection :: Frame3d (space @ units) defines -> Direction3d space
backwardDirection frame = Basis3d.backwardDirection (basis frame)

-- | Get the local upward direction of a frame.
upwardDirection :: Frame3d (space @ units) defines -> Direction3d space
upwardDirection frame = Basis3d.upwardDirection (basis frame)

-- | Get the local downward direction of a frame.
downwardDirection :: Frame3d (space @ units) defines -> Direction3d space
downwardDirection frame = Basis3d.downwardDirection (basis frame)

-- | Get the rightward axis of a frame.
rightwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
rightwardAxis frame = Axis3d (originPoint frame) (rightwardDirection frame)

-- | Get the leftward axis of a frame.
leftwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
leftwardAxis frame = Axis3d (originPoint frame) (leftwardDirection frame)

-- | Get the forward axis of a frame.
forwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
forwardAxis frame = Axis3d (originPoint frame) (forwardDirection frame)

-- | Get the backward axis of a frame.
backwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
backwardAxis frame = Axis3d (originPoint frame) (backwardDirection frame)

-- | Get the upward axis of a frame.
upwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
upwardAxis frame = Axis3d (originPoint frame) (upwardDirection frame)

-- | Get the downward axis of a frame.
downwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
downwardAxis frame = Axis3d (originPoint frame) (downwardDirection frame)

{-| Construct a locally forward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's forward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's upward direction.
-}
frontPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
frontPlane (Frame3d p0 (Basis3d r _ u)) = Plane3d p0 (PlanarBasis3d -r u)

{-| Construct a locally backward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's backward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's upward direction.
-}
backPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
backPlane (Frame3d p0 (Basis3d r _ u)) = Plane3d p0 (PlanarBasis3d r u)

{-| Construct a locally leftward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's leftward direction,
its X direction will be the frame's backward direction
and its Y direction will be frame's upward direction.
-}
leftPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
leftPlane (Frame3d p0 (Basis3d _ f u)) = Plane3d p0 (PlanarBasis3d -f u)

{-| Construct a locally rightward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's rightward direction,
its X direction will be the frame's forward direction
and its Y direction will be frame's upward direction.
-}
rightPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
rightPlane (Frame3d p0 (Basis3d _ f u)) = Plane3d p0 (PlanarBasis3d f u)

{-| Construct a locally upward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's upward direction,
its X direction will be the frame's rightward direction
and its Y direction will be frame's forward direction.
-}
topPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
topPlane (Frame3d p0 (Basis3d r f _)) = Plane3d p0 (PlanarBasis3d r f)

{-| Construct a locally downward-facing plane from a frame.

The returned plane will have the same origin point as the frame,
its normal direction will be the frame's downward direction,
its X direction will be the frame's leftward direction
and its Y direction will be frame's forward direction.
-}
bottomPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
bottomPlane (Frame3d p0 (Basis3d r f _)) = Plane3d p0 (PlanarBasis3d -r f)

-- | Construct a plane from its front plane.
fromFrontPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromFrontPlane (Plane3d p0 (PlanarBasis3d l u)) = Frame3d p0 (Basis3d -l (Unit3d (l `cross` u)) u)

-- | Construct a plane from its back plane.
fromBackPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromBackPlane (Plane3d p0 (PlanarBasis3d r u)) = Frame3d p0 (Basis3d r (Unit3d (u `cross` r)) u)

-- | Construct a plane from its left plane.
fromLeftPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromLeftPlane (Plane3d p0 (PlanarBasis3d b u)) = Frame3d p0 (Basis3d (Unit3d (u `cross` b)) -b u)

-- | Construct a plane from its right plane.
fromRightPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromRightPlane (Plane3d p0 (PlanarBasis3d f u)) = Frame3d p0 (Basis3d (Unit3d (f `cross` u)) f u)

-- | Construct a plane from its top plane.
fromTopPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromTopPlane (Plane3d p0 (PlanarBasis3d r f)) = Frame3d p0 (Basis3d r f (Unit3d (r `cross` f)))

-- | Construct a plane from its bottom plane.
fromBottomPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromBottomPlane (Plane3d p0 (PlanarBasis3d l f)) = Frame3d p0 (Basis3d -l f (Unit3d (f `cross` l)))

{-| Rotate a frame 180 degrees around its upward axis.

This reverses the frame's forward and rightward directions,
while leaving its upward direction unchanged.
-}
reverse :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
reverse (Frame3d p0 (Basis3d r f u)) = Frame3d p0 (Basis3d -r -f u)

-- | Move a frame in its own forward direction by the given distance.
offsetForwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetForwardBy distance = translateInOwn forwardDirection distance

-- | Move a frame in its own backward direction by the given distance.
offsetBackwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetBackwardBy distance = translateInOwn backwardDirection distance

-- | Move a frame in its own rightward direction by the given distance.
offsetRightwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetRightwardBy distance = translateInOwn rightwardDirection distance

-- | Move a frame in its own leftward direction by the given distance.
offsetLeftwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetLeftwardBy distance = translateInOwn leftwardDirection distance

-- | Move a frame in its own upward direction by the given distance.
offsetUpwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetUpwardBy distance = translateInOwn upwardDirection distance

-- | Move a frame in its own downward direction by the given distance.
offsetDownwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetDownwardBy distance = translateInOwn downwardDirection distance

{-| Rotate a frame counterclockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its leftward direction.
-}
turnLeftBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnLeftBy angle = rotateAroundOwn upwardAxis angle

{-| Rotate a frame clockwise around its own upward axis by the given angle.

This rotates the frame's forward direction toward its rightward direction.
-}
turnRightBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnRightBy angle = rotateAroundOwn upwardAxis -angle

{-| Rotate a frame counterclockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its rightward direction.
-}
rollRightBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollRightBy angle = rotateAroundOwn forwardAxis angle

{-| Rotate a frame clockwise around its own forward axis by the given angle.

This rotates the frame's upward direction toward its leftward direction.
-}
rollLeftBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollLeftBy angle = rotateAroundOwn forwardAxis -angle

{-| Rotate a frame counterclockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its upward direction.
-}
rotateUpBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateUpBy angle = rotateAroundOwn rightwardAxis angle

{-| Rotate a frame clockwise around its own rightward axis by the given angle.

This rotates the frame's forward direction toward its downward direction.
-}
rotateDownBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateDownBy angle = rotateAroundOwn rightwardAxis -angle

{-| Turn a frame left by 90 degrees.

The forward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
turnLeft :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnLeft = turnLeftBy Angle.halfPi

{-| Turn a frame right by 90 degrees.

The forward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
turnRight :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnRight = turnRightBy Angle.halfPi

{-| Roll a frame left by 90 degrees.

The upward direction of the returned frame
will be equal to the leftward direction of the original frame.
-}
rollLeft :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollLeft = rollLeftBy Angle.halfPi

{-| Roll a frame right by 90 degrees.

The upward direction of the returned frame
will be equal to the rightward direction of the original frame.
-}
rollRight :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollRight = rollRightBy Angle.halfPi

{-| Rotate a frame up by 90 degrees.

The forward direction of the returned frame
will be equal to the upward direction of the original frame.
-}
rotateUp :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateUp = rotateUpBy Angle.halfPi

{-| Rotate a frame down by 90 degrees.

The forward direction of the returned frame
will be equal to the downward direction of the original frame.
-}
rotateDown :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateDown = rotateDownBy Angle.halfPi

-- | Convert a frame defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ units) (Defines local1) ->
  Frame3d (local1 @ units) (Defines local2) ->
  Frame3d (global @ units) (Defines local2)
placeIn globalFrame frame =
  Frame3d
    (Point3d.placeIn globalFrame (originPoint frame))
    (Basis3d.placeIn (basis globalFrame) (basis frame))

-- | Convert a frame defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ units) (Defines local1) ->
  Frame3d (global @ units) (Defines local2) ->
  Frame3d (local1 @ units) (Defines local2)
relativeTo globalFrame frame =
  Frame3d
    (Point3d.relativeTo globalFrame (originPoint frame))
    (Basis3d.relativeTo (basis globalFrame) (basis frame))

{-| Compute the "inverse" of a given frame.

This is a frame that defines the current global coordinate system
in terms of the frame's local coordinate system,
instead of the other way around.
-}
inverse :: Frame3d (global @ units) (Defines local) -> Frame3d (local @ units) (Defines global)
inverse frame = identity |> relativeTo frame

-- | Move a frame to a new origin point.
moveTo ::
  Point3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
moveTo newOriginPoint (Frame3d _ b) = Frame3d newOriginPoint (Basis3d.coerce b)

-- | Apply the given transform to a frame.
transformBy ::
  Transform3d.Rigid (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
transformBy transform (Frame3d p0 b) =
  Frame3d (Point3d.transformBy transform p0) (Basis3d.transformBy transform b)

translateBy ::
  Vector3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d (space @ units) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d (space @ units) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAround = Transform3d.rotateAroundImpl transformBy

translateByOwn ::
  (Frame3d (space @ units) defines1 -> Vector3d (space @ units)) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn ::
  (Frame3d (space @ units) defines1 -> Direction3d space) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy

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
  Frame3d (local @ units) (Defines space) ->
  Frame3d (global @ units) (Defines space) ->
  Frame3d (global @ units) (Defines local)
align frame referenceFrame = inverse frame |> placeIn referenceFrame

{-| Compute the relative orientation of two parent frames in order to "mate" two child frames.

This is the same as 'align' except that the two child frames will end up facing towards each other
(with reversed forward directions, but the same upward directions)
instead of being aligned with each other.
-}
mate ::
  Frame3d (local @ units) (Defines space) ->
  Frame3d (global @ units) (Defines space) ->
  Frame3d (global @ units) (Defines local)
mate frame referenceFrame = align (reverse frame) referenceFrame
