module OpenSolid.Plane3d
  ( Plane3d (Plane3d, originPoint, orientation)
  , fromPointAndNormal
  , fromXAxis
  , fromYAxis
  , coerce
  , originPoint
  , orientation
  , normalDirection
  , normalAxis
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , flip
  , moveTo
  , placeIn
  , relativeTo
  , offsetBy
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Length (Length)
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d (Axis3d), Frame3d, Plane3d (Plane3d, orientation, originPoint))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)

{-| Construct a plane with the given origin point and normal direction.

Both the X and Y directions of the returned plane will be perpendicular to the given direction
(and, of course, they will be perpendicular to each other),
but otherwise they will be chosen arbitrarily.
-}
fromPointAndNormal :: Point3d space -> Direction3d space -> Plane3d space defines
fromPointAndNormal p n = Plane3d p (PlaneOrientation3d.fromNormalDirection n)

-- | Construct a plane having the given X axis, with an arbitrarily-chosen Y direction.
fromXAxis :: Axis3d space -> Plane3d space defines
fromXAxis (Axis3d p0 dx) = Plane3d p0 (PlaneOrientation3d.fromXDirection dx)

-- | Construct a plane having the given Y axis, with an arbitrarily-chosen X direction.
fromYAxis :: Axis3d space -> Plane3d space defines
fromYAxis (Axis3d p0 dy) = Plane3d p0 (PlaneOrientation3d.fromYDirection dy)

coerce :: Plane3d space1 defines1 -> Plane3d space2 defines2
coerce (Plane3d p o) = Plane3d (Point3d.coerce p) (PlaneOrientation3d.coerce o)

{-| Get the origin point of a plane.

This is the 3D point corresponding to (0,0) in the plane's local coordinates.
-}
originPoint :: Plane3d space defines -> Point3d space
originPoint = (.originPoint)

orientation :: Plane3d space defines -> PlaneOrientation3d space
orientation = (.orientation)

-- | Get the normal direction of a plane.
normalDirection :: Plane3d space defines -> Direction3d space
normalDirection = (.normalDirection)

{-| Construct an axis normal (perpendicular) to a plane.

The origin point of the axis will be the origin point of the plane,
and the direction of the axis will be the normal direction of the plane.
-}
normalAxis :: Plane3d space defines -> Axis3d space
normalAxis = (.normalAxis)

-- | Get the X direction of a plane.
xDirection :: Plane3d space defines -> Direction3d space
xDirection = (.xDirection)

-- | Get the Y direction of a plane.
yDirection :: Plane3d space defines -> Direction3d space
yDirection = (.yDirection)

{-| Get the X axis of a plane.

This is an axis formed from the plane's origin point and X direction.
-}
xAxis :: Plane3d space defines -> Axis3d space
xAxis = (.xAxis)

{-| Get the Y axis of a plane.

This is an axis formed from the plane's origin point and Y direction.
-}
yAxis :: Plane3d space defines -> Axis3d space
yAxis = (.yAxis)

{-| Move a plane so that its origin point is the given point.

The orientation of the plane will remain unchanged.
-}
moveTo :: Point3d space -> Plane3d space defines -> Plane3d space defines
moveTo p0 plane = Plane3d p0 (orientation plane)

-- | Convert a plane defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d global local -> Plane3d local defines -> Plane3d global defines
placeIn frame (Plane3d p o) =
  Plane3d (Point3d.placeIn frame p) (PlaneOrientation3d.placeIn frame o)

-- | Convert a plane defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3d global local -> Plane3d global defines -> Plane3d local defines
relativeTo frame (Plane3d p o) =
  Plane3d (Point3d.relativeTo frame p) (PlaneOrientation3d.relativeTo frame o)

-- | Offset a plane in its normal direction by the given distance.
offsetBy :: Length -> Plane3d space defines -> Plane3d space defines
offsetBy distance plane = translateIn (normalDirection plane) distance plane

{-| Flip a plane such that its normal and X directions are reversed.

The Y direction will remain constant.
-}
flip :: Plane3d space defines -> Plane3d space defines
flip (Plane3d p o) = Plane3d p (PlaneOrientation3d.flip o)

transformBy :: Transform3d.Rigid space -> Plane3d space defines -> Plane3d space defines
transformBy transform (Plane3d p o) =
  Plane3d (Point3d.transformBy transform p) (PlaneOrientation3d.transformBy transform o)

translateBy :: Vector3d Meters space -> Plane3d space defines -> Plane3d space defines
translateBy = Transform3d.translateByImpl transformBy

translateIn :: Direction3d space -> Length -> Plane3d space defines -> Plane3d space defines
translateIn = Transform3d.translateInImpl transformBy

translateAlong :: Axis3d space -> Length -> Plane3d space defines -> Plane3d space defines
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround :: Axis3d space -> Angle -> Plane3d space defines -> Plane3d space defines
rotateAround = Transform3d.rotateAroundImpl transformBy
