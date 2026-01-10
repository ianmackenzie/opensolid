{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Point3d
  ( Point3d
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
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Length (Length)
import OpenSolid.Point2D (Point2D, pattern Point2D)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d, Position3d)
  , Transform3d (Transform3d)
  , Vector3d
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Unboxed.Math
import OpenSolid.Vector3d qualified as Vector3d

-- | Get the XYZ coordinates of a point, given an XYZ coordinate convention to use.
{-# INLINE coordinates #-}
coordinates :: Convention3d -> Point3d space -> (Length, Length, Length)
coordinates convention (Position3d vector) = Vector3d.components convention vector

{-| Get the XYZ coordinates of a point using a Z-up coordinate convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
{-# INLINE zUpCoordinates #-}
zUpCoordinates :: Point3d space -> (Length, Length, Length)
zUpCoordinates (Position3d vector) = Vector3d.zUpComponents vector

{-# INLINE zUpCoordinates# #-}
zUpCoordinates# :: Point3d space -> (# Double#, Double#, Double# #)
zUpCoordinates# (Position3d vector) = Vector3d.zUpComponents# vector

{-| Get the XYZ coordinates of a point using a Y-up coordinate convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
{-# INLINE yUpCoordinates #-}
yUpCoordinates :: Point3d space -> (Length, Length, Length)
yUpCoordinates (Position3d vector) = Vector3d.yUpComponents vector

{-# INLINE yUpCoordinates# #-}
yUpCoordinates# :: Point3d space -> (# Double#, Double#, Double# #)
yUpCoordinates# (Position3d vector) = Vector3d.yUpComponents# vector

-- | Construct a point the given distance along the given axis.
along :: Axis3d space -> Length -> Point3d space
along (Axis3d originPoint direction) distance = do
  let Point3d oR oF oU = originPoint
  let Direction3d dR dF dU = direction
  Point3d
    (oR .+. dR .*. distance)
    (oF .+. dF .*. distance)
    (oU .+. dU .*. distance)

-- | Construct a point on the given plane, at the given position within the plane.
on :: Plane3d global local -> Point2D Meters local -> Point3d global
on (Plane3d originPoint (PlaneOrientation3d i j)) (Point2D pX pY) = do
  let Point3d oR oF oU = originPoint
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  Point3d
    (oR .+. pX .*. iR .+. pY .*. jR)
    (oF .+. pX .*. iF .+. pY .*. jF)
    (oU .+. pX .*. iU .+. pY .*. jU)

{-# INLINE coerce #-}
coerce :: Point3d space1 -> Point3d space2
coerce (Position3d p) = Position3d (Vector3d.coerce p)

-- | Construct a point from its XYZ coordinates, given the coordinate convention to use.
{-# INLINE xyz #-}
xyz :: Convention3d -> (Length, Length, Length) -> Point3d space
xyz convention givenCoordinates = Position3d (Vector3d.xyz convention givenCoordinates)

{-| Construct a point from its XYZ coordinates, using a Z-up convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
zUp :: Length -> Length -> Length -> Point3d space
zUp pX pY pZ = Point3d pX pY pZ

{-| Construct a point from its XYZ coordinates, using a Y-up convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
yUp :: Length -> Length -> Length -> Point3d space
yUp pX pY pZ = Point3d (negative pX) pZ pY

interpolateFrom :: Point3d space -> Point3d space -> Number -> Point3d space
interpolateFrom (Position3d p1) (Position3d p2) t = Position3d (Vector3d.interpolateFrom p1 p2 t)

-- | Find the midpoint between two points.
midpoint :: Point3d space -> Point3d space -> Point3d space
midpoint (Position3d p1) (Position3d p2) = Position3d (Vector3d.midpoint p1 p2)

-- | Compute the distance from one point to another.
{-# INLINE distanceFrom #-}
distanceFrom :: Point3d space -> Point3d space -> Length
distanceFrom p1 p2 = Quantity# (distanceFrom# p1 p2)

{-# INLINE distanceFrom# #-}
distanceFrom# :: Point3d space -> Point3d space -> Double#
distanceFrom#
  (Point3d (Quantity# x1#) (Quantity# y1#) (Quantity# z1#))
  (Point3d (Quantity# x2#) (Quantity# y2#) (Quantity# z2#)) =
    hypot3# (x2# -# x1#) (y2# -# y1#) (z2# -# z1#)

{-| Compute the (signed) distance of a point along an axis.

This is the position along the axis of the given point projected onto the axis.
-}
distanceAlong :: Axis3d space -> Point3d space -> Length
distanceAlong (Axis3d p0 d) p = (p .-. p0) `dot` d

-- | Convert a point defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d global local -> Point3d local -> Point3d global
placeIn (Frame3d p0 (Orientation3d i j k)) (Point3d px py pz) =
  p0 .+. px .*. i .+. py .*. j .+. pz .*. k

-- | Convert a point defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3d global local -> Point3d global -> Point3d local
relativeTo (Frame3d p0 (Orientation3d i j k)) p =
  let d = p .-. p0 in Point3d (d `dot` i) (d `dot` j) (d `dot` k)

-- | Project a point onto a plane.
projectOnto :: Plane3d global local -> Point3d global -> Point3d global
projectOnto plane point =
  point .-. Vector3d.projectionIn plane.normalDirection (point .-. plane.originPoint)

{-| Project a point *into* a plane.

Conceptualy, this projects the point onto the plane in 3D,
then expresses the projected point in 2D planar XY coordinates.
-}
projectInto :: Plane3d space local -> Point3d space -> Point2D Meters local
projectInto (Plane3d p0 (PlaneOrientation3d i j)) p =
  let d = p .-. p0 in Point2D (d `dot` i) (d `dot` j)

transformBy :: Transform3d tag space -> Point3d space -> Point3d space
transformBy transform (Point3d px py pz) = do
  let (Transform3d p0 vx vy vz) = transform
  p0 .+. px .*. vx .+. py .*. vy .+. pz .*. vz

translateBy :: Vector3d Meters space -> Point3d space -> Point3d space
translateBy = Transform3d.translateByImpl transformBy

translateIn :: Direction3d space -> Length -> Point3d space -> Point3d space
translateIn = Transform3d.translateInImpl transformBy

translateAlong :: Axis3d space -> Length -> Point3d space -> Point3d space
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround :: Axis3d space -> Angle -> Point3d space -> Point3d space
rotateAround = Transform3d.rotateAroundImpl transformBy

mirrorAcross :: Plane3d global local -> Point3d global -> Point3d global
mirrorAcross = Transform3d.mirrorAcrossImpl transformBy

scaleAbout :: Point3d space -> Number -> Point3d space -> Point3d space
scaleAbout = Transform3d.scaleAboutImpl transformBy

scaleAlong :: Axis3d space -> Number -> Point3d space -> Point3d space
scaleAlong = Transform3d.scaleAlongImpl transformBy
