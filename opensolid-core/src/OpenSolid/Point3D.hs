{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Point3D
  ( Point3D
  , coordinates
  , zUpCoordinates
  , zUpCoordinates#
  , yUpCoordinates
  , yUpCoordinates#
  , on
  , along
  , coerce
  , xyz
  , zUp
  , yUp
  , midpoint
  , interpolateFrom
  , distanceFrom
  , distanceFrom#
  , distanceAlong
  , placeIn
  , relativeTo
  , projectOnto
  , projectInto
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Point2D (Point2D, pattern Point2D)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D)
  , Frame3D (Frame3D)
  , Orientation3D (Orientation3D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point3D (Point3D, Position3D)
  , Transform3D (Transform3D)
  , Vector3D
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Unboxed.Math
import OpenSolid.Vector3D qualified as Vector3D

-- | Get the XYZ coordinates of a point, given an XYZ coordinate convention to use.
{-# INLINE coordinates #-}
coordinates :: Convention3D -> Point3D space -> (Length, Length, Length)
coordinates convention (Position3D vector) = Vector3D.components convention vector

{-| Get the XYZ coordinates of a point using a Z-up coordinate convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
{-# INLINE zUpCoordinates #-}
zUpCoordinates :: Point3D space -> (Length, Length, Length)
zUpCoordinates (Position3D vector) = Vector3D.zUpComponents vector

{-# INLINE zUpCoordinates# #-}
zUpCoordinates# :: Point3D space -> (# Double#, Double#, Double# #)
zUpCoordinates# (Position3D vector) = Vector3D.zUpComponents# vector

{-| Get the XYZ coordinates of a point using a Y-up coordinate convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
{-# INLINE yUpCoordinates #-}
yUpCoordinates :: Point3D space -> (Length, Length, Length)
yUpCoordinates (Position3D vector) = Vector3D.yUpComponents vector

{-# INLINE yUpCoordinates# #-}
yUpCoordinates# :: Point3D space -> (# Double#, Double#, Double# #)
yUpCoordinates# (Position3D vector) = Vector3D.yUpComponents# vector

-- | Construct a point the given distance along the given axis.
along :: Axis3D space -> Length -> Point3D space
along (Axis3D originPoint direction) distance = do
  let Point3D oR oF oU = originPoint
  let Direction3D dR dF dU = direction
  Point3D
    (oR + dR * distance)
    (oF + dF * distance)
    (oU + dU * distance)

-- | Construct a point on the given plane, at the given position within the plane.
on :: Plane3D global local -> Point2D Meters local -> Point3D global
on (Plane3D originPoint (PlaneOrientation3D i j)) (Point2D pX pY) = do
  let Point3D oR oF oU = originPoint
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  Point3D
    (oR + pX * iR + pY * jR)
    (oF + pX * iF + pY * jF)
    (oU + pX * iU + pY * jU)

{-# INLINE coerce #-}
coerce :: Point3D space1 -> Point3D space2
coerce (Position3D p) = Position3D (Vector3D.coerce p)

-- | Construct a point from its XYZ coordinates, given the coordinate convention to use.
{-# INLINE xyz #-}
xyz :: Convention3D -> (Length, Length, Length) -> Point3D space
xyz convention givenCoordinates = Position3D (Vector3D.xyz convention givenCoordinates)

{-| Construct a point from its XYZ coordinates, using a Z-up convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
zUp :: Length -> Length -> Length -> Point3D space
zUp pX pY pZ = Point3D pX pY pZ

{-| Construct a point from its XYZ coordinates, using a Y-up convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
yUp :: Length -> Length -> Length -> Point3D space
yUp pX pY pZ = Point3D -pX pZ pY

interpolateFrom :: Point3D space -> Point3D space -> Number -> Point3D space
interpolateFrom (Position3D p1) (Position3D p2) t = Position3D (Vector3D.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point3D space -> Point3D space -> Point3D space
midpoint (Position3D p1) (Position3D p2) = Position3D (Vector3D.midpoint p1 p2)

-- | Compute the distance from one point to another.
{-# INLINE distanceFrom #-}
distanceFrom :: Point3D space -> Point3D space -> Length
distanceFrom p1 p2 = Quantity# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point3D space -> Point3D space -> Double#
distanceFrom#
  (Point3D (Quantity# x1#) (Quantity# y1#) (Quantity# z1#))
  (Point3D (Quantity# x2#) (Quantity# y2#) (Quantity# z2#)) =
    hypot3# (x2# -# x1#) (y2# -# y1#) (z2# -# z1#)

{-| Compute the (signed) distance of a point along an axis.

This is the position along the axis of the given point projected onto the axis.
-}
distanceAlong :: Axis3D space -> Point3D space -> Length
distanceAlong (Axis3D p0 d) p = (p - p0) `dot` d

-- | Convert a point defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Point3D local -> Point3D global
placeIn (Frame3D p0 (Orientation3D i j k)) (Point3D px py pz) =
  p0 + px * i + py * j + pz * k

-- | Convert a point defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Point3D global -> Point3D local
relativeTo (Frame3D p0 (Orientation3D i j k)) p =
  let d = p - p0 in Point3D (d `dot` i) (d `dot` j) (d `dot` k)

-- | Project a point onto a plane.
projectOnto :: Plane3D global local -> Point3D global -> Point3D global
projectOnto plane point =
  point - Vector3D.projectionIn plane.normalDirection (point - plane.originPoint)

{-| Project a point *into* a plane.

Conceptualy, this projects the point onto the plane in 3D,
then expresses the projected point in 2D planar XY coordinates.
-}
projectInto :: Plane3D space local -> Point3D space -> Point2D Meters local
projectInto (Plane3D p0 (PlaneOrientation3D i j)) p =
  let d = p - p0 in Point2D (d `dot` i) (d `dot` j)

transformBy :: Transform3D tag space -> Point3D space -> Point3D space
transformBy transform (Point3D px py pz) = do
  let (Transform3D p0 vx vy vz) = transform
  p0 + px * vx + py * vy + pz * vz

translateBy :: Vector3D Meters space -> Point3D space -> Point3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Point3D space -> Point3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Point3D space -> Point3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Point3D space -> Point3D space
rotateAround = Transform3D.rotateAroundImpl transformBy

mirrorAcross :: Plane3D global local -> Point3D global -> Point3D global
mirrorAcross = Transform3D.mirrorAcrossImpl transformBy

scaleAbout :: Point3D space -> Number -> Point3D space -> Point3D space
scaleAbout = Transform3D.scaleAboutImpl transformBy

scaleAlong :: Axis3D space -> Number -> Point3D space -> Point3D space
scaleAlong = Transform3D.scaleAlongImpl transformBy
