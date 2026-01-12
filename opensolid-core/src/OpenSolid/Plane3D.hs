module OpenSolid.Plane3D
  ( Plane3D (Plane3D, originPoint, orientation)
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
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.PlaneOrientation3D (PlaneOrientation3D)
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3D (Axis3D), Frame3D, Plane3D (Plane3D, orientation, originPoint))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)

{-| Construct a plane with the given origin point and normal direction.

Both the X and Y directions of the returned plane will be perpendicular to the given direction
(and, of course, they will be perpendicular to each other),
but otherwise they will be chosen arbitrarily.
-}
fromPointAndNormal :: Point3D global -> Direction3D global -> Plane3D global local
fromPointAndNormal p n = Plane3D p (PlaneOrientation3D.fromNormalDirection n)

-- | Construct a plane having the given X axis, with an arbitrarily-chosen Y direction.
fromXAxis :: Axis3D global -> Plane3D global local
fromXAxis (Axis3D p0 dx) = Plane3D p0 (PlaneOrientation3D.fromXDirection dx)

-- | Construct a plane having the given Y axis, with an arbitrarily-chosen X direction.
fromYAxis :: Axis3D global -> Plane3D global local
fromYAxis (Axis3D p0 dy) = Plane3D p0 (PlaneOrientation3D.fromYDirection dy)

coerce :: Plane3D global1 local1 -> Plane3D global2 local2
coerce (Plane3D p o) = Plane3D (Point3D.coerce p) (PlaneOrientation3D.coerce o)

{-| Get the origin point of a plane.

This is the 3D point corresponding to (0,0) in the plane's local coordinates.
-}
originPoint :: Plane3D global local -> Point3D global
originPoint = (.originPoint)

orientation :: Plane3D global local -> PlaneOrientation3D global
orientation = (.orientation)

-- | Get the normal direction of a plane.
normalDirection :: Plane3D global local -> Direction3D global
normalDirection = (.normalDirection)

{-| Construct an axis normal (perpendicular) to a plane.

The origin point of the axis will be the origin point of the plane,
and the direction of the axis will be the normal direction of the plane.
-}
normalAxis :: Plane3D global local -> Axis3D global
normalAxis = (.normalAxis)

-- | Get the X direction of a plane.
xDirection :: Plane3D global local -> Direction3D global
xDirection = (.xDirection)

-- | Get the Y direction of a plane.
yDirection :: Plane3D global local -> Direction3D global
yDirection = (.yDirection)

{-| Get the X axis of a plane.

This is an axis formed from the plane's origin point and X direction.
-}
xAxis :: Plane3D global local -> Axis3D global
xAxis = (.xAxis)

{-| Get the Y axis of a plane.

This is an axis formed from the plane's origin point and Y direction.
-}
yAxis :: Plane3D global local -> Axis3D global
yAxis = (.yAxis)

{-| Move a plane so that its origin point is the given point.

The orientation of the plane will remain unchanged.
-}
moveTo :: Point3D global -> Plane3D global local -> Plane3D global local
moveTo p0 plane = Plane3D p0 (orientation plane)

-- | Convert a plane defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D space1 space2 -> Plane3D space2 space3 -> Plane3D space1 space3
placeIn frame (Plane3D p o) =
  Plane3D (Point3D.placeIn frame p) (PlaneOrientation3D.placeIn frame o)

-- | Convert a plane defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D space1 space2 -> Plane3D space1 space3 -> Plane3D space2 space3
relativeTo frame (Plane3D p o) =
  Plane3D (Point3D.relativeTo frame p) (PlaneOrientation3D.relativeTo frame o)

-- | Offset a plane in its normal direction by the given distance.
offsetBy :: Length -> Plane3D global local -> Plane3D global local
offsetBy distance plane = translateIn (normalDirection plane) distance plane

{-| Flip a plane such that its normal and X directions are reversed.

The Y direction will remain constant.
-}
flip :: Plane3D global local -> Plane3D global local
flip (Plane3D p o) = Plane3D p (PlaneOrientation3D.flip o)

transformBy :: Transform3D.Rigid global -> Plane3D global local -> Plane3D global local
transformBy transform (Plane3D p o) =
  Plane3D (Point3D.transformBy transform p) (PlaneOrientation3D.transformBy transform o)

translateBy :: Vector3D Meters global -> Plane3D global local -> Plane3D global local
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D global -> Length -> Plane3D global local -> Plane3D global local
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D global -> Length -> Plane3D global local -> Plane3D global local
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D global -> Angle -> Plane3D global local -> Plane3D global local
rotateAround = Transform3D.rotateAroundImpl transformBy
