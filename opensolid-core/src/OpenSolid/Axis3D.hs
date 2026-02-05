module OpenSolid.Axis3D
  ( Axis3D (Axis3D, originPoint, direction)
  , coerce
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

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3D (Axis3D, direction, originPoint), Frame3D, Plane3D (Plane3D))
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3D (Transform3D)

{-# INLINE coerce #-}
coerce :: Axis3D space1 -> Axis3D space2
coerce (Axis3D p0 d) = Axis3D (Point3D.coerce p0) (Direction3D.coerce d)

-- | Get the origin point of an axis.
originPoint :: Axis3D space -> Point3D space
originPoint (Axis3D p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis3D space -> Direction3D space
direction (Axis3D _ d) = d

through :: Point3D space -> Direction3D space -> Axis3D space
through = Axis3D

{-| Construct a plane normal (perpendicular) to the given axis.

The origin point of the plane will be the origin point of the axis,
and the normal direction of the plane will be the direction of the axis.
The X and Y directions of the plane will be chosen arbitrarily.
-}
normalPlane :: Axis3D global -> Plane3D global local
normalPlane (Axis3D p0 d) = Plane3D p0 (PlaneOrientation3D.fromNormalDirection d)

{-| Move an axis so that its origin point is the given point.

The direction of the axis will remain unchanged.
-}
moveTo :: Point3D space -> Axis3D space -> Axis3D space
moveTo newOriginPoint axis = Axis3D newOriginPoint (direction axis)

{-| Reverse an axis (negate/reverse its direction).

The origin point of the axis will remain unchanged.
-}
reverse :: Axis3D space -> Axis3D space
reverse (Axis3D p0 d) = Axis3D p0 -d

transformBy :: Transform.IsOrthonormal tag => Transform3D tag space -> Axis3D space -> Axis3D space
transformBy transform axis = do
  let transformedOriginPoint = Point3D.transformBy transform (originPoint axis)
  let transformedDirection = Direction3D.transformBy transform (direction axis)
  Axis3D transformedOriginPoint transformedDirection

-- | Convert an axis defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Axis3D local -> Axis3D global
placeIn frame (Axis3D p0 d) =
  Axis3D (Point3D.placeIn frame p0) (Direction3D.placeIn frame d)

-- | Convert an axis defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Axis3D global -> Axis3D local
relativeTo frame (Axis3D p0 d) =
  Axis3D (Point3D.relativeTo frame p0) (Direction3D.relativeTo frame d)
