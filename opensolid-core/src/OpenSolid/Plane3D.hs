module OpenSolid.Plane3D
  ( Plane3D (Plane3D, originPoint, orientation)
  , fromPointAndNormal
  , fromXAxis
  , fromYAxis
  , fromNormalAxis
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
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis3D (Axis3D (Axis3D))
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.PlaneOrientation3D (PlaneOrientation3D)
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3D, Plane3D (Plane3D, orientation, originPoint))
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)

{-| Construct a plane with the given origin point and normal direction.

Both the X and Y directions of the returned plane will be perpendicular to the given direction
(and, of course, they will be perpendicular to each other),
but otherwise they will be chosen arbitrarily.
-}
fromPointAndNormal :: Point3D space -> Direction3D space -> Plane3D space
fromPointAndNormal p n = Plane3D p (PlaneOrientation3D.fromNormalDirection n)

-- | Construct a plane having the given X axis, with an arbitrarily-chosen Y direction.
fromXAxis :: Axis3D space -> Plane3D space
fromXAxis (Axis3D p0 dx) = Plane3D p0 (PlaneOrientation3D.fromXDirection dx)

-- | Construct a plane having the given Y axis, with an arbitrarily-chosen X direction.
fromYAxis :: Axis3D space -> Plane3D space
fromYAxis (Axis3D p0 dy) = Plane3D p0 (PlaneOrientation3D.fromYDirection dy)

{-| Construct a plane having the given normal axis, with arbitrary X and Y directions.

(The X and Y directions will, of course, be perpendicular to each other and the normal direction.)
-}
fromNormalAxis :: Axis3D space -> Plane3D space
fromNormalAxis (Axis3D p0 n) = fromPointAndNormal p0 n

coerce :: Plane3D space1 -> Plane3D space2
coerce (Plane3D p o) = Plane3D (Point3D.coerce p) (PlaneOrientation3D.coerce o)

{-| Get the origin point of a plane.

This is the 3D point corresponding to (0,0) in the plane's local coordinates.
-}
originPoint :: Plane3D space -> Point3D space
originPoint = (.originPoint)

orientation :: Plane3D space -> PlaneOrientation3D space
orientation = (.orientation)

-- | Get the normal direction of a plane.
normalDirection :: Plane3D space -> Direction3D space
normalDirection plane = PlaneOrientation3D.normalDirection (orientation plane)

{-| Construct an axis normal (perpendicular) to a plane.

The origin point of the axis will be the origin point of the plane,
and the direction of the axis will be the normal direction of the plane.
-}
normalAxis :: Plane3D space -> Axis3D space
normalAxis plane = Axis3D (originPoint plane) (normalDirection plane)

-- | Get the X direction of a plane.
xDirection :: Plane3D space -> Direction3D space
xDirection plane = PlaneOrientation3D.xDirection (orientation plane)

-- | Get the Y direction of a plane.
yDirection :: Plane3D space -> Direction3D space
yDirection plane = PlaneOrientation3D.yDirection (orientation plane)

{-| Get the X axis of a plane.

This is an axis formed from the plane's origin point and X direction.
-}
xAxis :: Plane3D space -> Axis3D space
xAxis plane = Axis3D (originPoint plane) (xDirection plane)

{-| Get the Y axis of a plane.

This is an axis formed from the plane's origin point and Y direction.
-}
yAxis :: Plane3D space -> Axis3D space
yAxis plane = Axis3D (originPoint plane) (yDirection plane)

{-| Move a plane so that its origin point is the given point.

The orientation of the plane will remain unchanged.
-}
moveTo :: Point3D space -> Plane3D space -> Plane3D space
moveTo p0 plane = Plane3D p0 (orientation plane)

-- | Convert a plane defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Plane3D local -> Plane3D global
placeIn frame (Plane3D p o) =
  Plane3D (Point3D.placeIn frame p) (PlaneOrientation3D.placeIn frame o)

-- | Convert a plane defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Plane3D global -> Plane3D local
relativeTo frame (Plane3D p o) =
  Plane3D (Point3D.relativeTo frame p) (PlaneOrientation3D.relativeTo frame o)

-- | Offset a plane in its normal direction by the given distance.
offsetBy :: Length -> Plane3D space -> Plane3D space
offsetBy distance plane = translateIn (normalDirection plane) distance plane

{-| Flip a plane such that its normal and X directions are reversed.

The Y direction will remain constant.
-}
flip :: Plane3D space -> Plane3D space
flip (Plane3D p o) = Plane3D p (PlaneOrientation3D.flip o)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3D tag space ->
  Plane3D space ->
  Plane3D space
transformBy transform (Plane3D p o) =
  Plane3D (Point3D.transformBy transform p) (PlaneOrientation3D.transformBy transform o)

translateBy :: Vector3D Meters space -> Plane3D space -> Plane3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Plane3D space -> Plane3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Plane3D space -> Plane3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Plane3D space -> Plane3D space
rotateAround = Transform3D.rotateAroundImpl transformBy

mirrorAcross :: Plane3D space -> Plane3D space -> Plane3D space
mirrorAcross = Transform3D.mirrorAcrossImpl transformBy
