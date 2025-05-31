module OpenSolid.Axis3d
  ( Axis3d (Axis3d)
  , coerce
  , erase
  , originPoint
  , direction
  , through
  , normalPlane
  , moveTo
  , reverse
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d (Axis3d), Frame3d (Frame3d), Plane3d (Plane3d))
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)

{-# INLINE coerce #-}
coerce :: Axis3d (space1 @ units1) -> Axis3d (space2 @ units2)
coerce (Axis3d p0 d) = Axis3d (Point3d.coerce p0) (Direction3d.coerce d)

erase :: Axis3d (space @ units) -> Axis3d (space @ Unitless)
erase = coerce

-- | Get the origin point of an axis.
originPoint :: Axis3d (space @ units) -> Point3d (space @ units)
originPoint (Axis3d p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis3d (space @ units) -> Direction3d space
direction (Axis3d _ d) = d

through :: Point3d (space @ units) -> Direction3d space -> Axis3d (space @ units)
through = Axis3d

{-| Construct a plane normal (perpendicular) to the given axis.

The origin point of the plane will be the origin point of the axis,
and the normal direction of the plane will be the direction of the axis.
The X and Y directions of the plane will be chosen arbitrarily.
-}
normalPlane :: Axis3d (space @ units) -> Plane3d (space @ units) defines
normalPlane (Axis3d p0 d) = Plane3d p0 (PlaneOrientation3d.fromNormalDirection d)

{-| Move an axis so that its origin point is the given point.

The direction of the axis will remain unchanged.
-}
moveTo :: Point3d (space @ units) -> Axis3d (space @ units) -> Axis3d (space @ units)
moveTo newOriginPoint axis = Axis3d newOriginPoint (direction axis)

{-| Reverse an axis (negate/reverse its direction).

The origin point of the axis will remain unchanged.
-}
reverse :: Axis3d (space @ units) -> Axis3d (space @ units)
reverse (Axis3d p0 d) = Axis3d p0 -d

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ units) ->
  Axis3d (space @ units) ->
  Axis3d (space @ units)
transformBy transform axis = do
  let transformedOriginPoint = Point3d.transformBy transform (originPoint axis)
  let transformedDirection = Direction3d.transformBy transform (direction axis)
  Axis3d transformedOriginPoint transformedDirection

-- | Convert an axis defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Axis3d (local @ units) ->
  Axis3d (global @ units)
placeIn frame (Axis3d p0 d) = do
  let Frame3d _ orientation = frame
  Axis3d (Point3d.placeIn frame p0) (Direction3d.placeIn orientation d)

-- | Convert an axis defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Axis3d (global @ units) ->
  Axis3d (local @ units)
relativeTo frame (Axis3d p0 d) = do
  let Frame3d _ orientation = frame
  Axis3d (Point3d.relativeTo frame p0) (Direction3d.relativeTo orientation d)
