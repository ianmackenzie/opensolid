module OpenSolid.Plane3d
  ( Plane3d (Plane3d)
  , through
  , xy
  , yx
  , zx
  , xz
  , yz
  , zy
  , fromXAxis
  , fromYAxis
  , originPoint
  , normalDirection
  , normalAxis
  , xnPlane
  , nxPlane
  , ynPlane
  , nyPlane
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , flipX
  , flipY
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
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  , mirrorAcrossOwn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d (Axis3d), Frame3d, Plane3d (Plane3d), Transform3d)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)

through :: Point3d (space @ units) -> Direction3d space -> Plane3d (space @ units) defines
through p0 n = Plane3d p0 (PlanarBasis3d.fromNormalDirection n)

{-| The XY plane.

A plane whose X direction is the global X direction
and whose Y direction is the global Y direction.
-}
xy :: Plane3d (space @ units) (Defines local)
xy = Plane3d Point3d.origin PlanarBasis3d.xy

{-| The YX plane.

A plane whose X direction is the global Y direction
and whose Y direction is the global X direction.
-}
yx :: Plane3d (space @ units) (Defines local)
yx = Plane3d Point3d.origin PlanarBasis3d.yx

{-| The ZX plane.

A plane whose X direction is the global Z direction
and whose Y direction is the global X direction.
-}
zx :: Plane3d (space @ units) (Defines local)
zx = Plane3d Point3d.origin PlanarBasis3d.zx

{-| The XZ plane.

A plane whose X direction is the global X direction
and whose Y direction is the global Z direction.
-}
xz :: Plane3d (space @ units) (Defines local)
xz = Plane3d Point3d.origin PlanarBasis3d.xz

{-| The YZ plane.

A plane whose X direction is the global Y direction
and whose Y direction is the global Z direction.
-}
yz :: Plane3d (space @ units) (Defines local)
yz = Plane3d Point3d.origin PlanarBasis3d.yz

{-| The ZY plane.

A plane whose X direction is the global Z direction
and whose Y direction is the global Y direction.
-}
zy :: Plane3d (space @ units) (Defines local)
zy = Plane3d Point3d.origin PlanarBasis3d.zy

{-| Construct a plane having the given X axis.

A perpendicular Y direction will be chosen arbitrarily.
-}
fromXAxis :: Axis3d (space @ units) -> Plane3d (space @ units) defines
fromXAxis (Axis3d p0 d) = Plane3d p0 (PlanarBasis3d.fromXDirection d)

{-| Construct a plane having the given Y axis.

A perpendicular X direction will be chosen arbitrarily.
-}
fromYAxis :: Axis3d (space @ units) -> Plane3d (space @ units) defines
fromYAxis (Axis3d p0 d) = Plane3d p0 (PlanarBasis3d.fromYDirection d)

{-| Get the origin point of a plane.

This is the 3D point corresponding to (0,0) in the plane's local coordinates.
-}
originPoint :: Plane3d (space @ units) defines -> Point3d (space @ units)
originPoint (Plane3d p0 _) = p0

-- | Get the normal direction of a plane.
normalDirection :: Plane3d (space @ units) defines -> Direction3d space
normalDirection (Plane3d _ basis) = PlanarBasis3d.normalDirection basis

{-| Construct an axis normal (perpendicular) to a plane.

The origin point of the axis will be the origin point of the plane,
and the direction of the axis will be the normal direction of the plane.
-}
normalAxis :: Plane3d (space @ units) defines -> Axis3d (space @ units)
normalAxis plane = Axis3d (originPoint plane) (normalDirection plane)

{-| Construct a plane from the X and normal directions of the given plane.

The returned plane will have the same origin point as the original plane.
-}
xnPlane :: Plane3d (space @ units) defines1 -> Plane3d (space @ units) defines2
xnPlane (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.xnBasis b)

{-| Construct a plane from the normal and X directions of the given plane.

The returned plane will have the same origin point as the original plane.
-}
nxPlane :: Plane3d (space @ units) defines1 -> Plane3d (space @ units) defines2
nxPlane (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.nxBasis b)

{-| Construct a plane from the Y and normal directions of the given plane.

The returned plane will have the same origin point as the original plane.
-}
ynPlane :: Plane3d (space @ units) defines1 -> Plane3d (space @ units) defines2
ynPlane (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.ynBasis b)

{-| Construct a plane from the normal and Y directions of the given plane.

The returned plane will have the same origin point as the original plane.
-}
nyPlane :: Plane3d (space @ units) defines1 -> Plane3d (space @ units) defines2
nyPlane (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.nyBasis b)

-- | Get the X direction of a plane.
xDirection :: Plane3d (space @ units) defines -> Direction3d space
xDirection (Plane3d _ basis) = PlanarBasis3d.xDirection basis

-- | Get the Y direction of a plane.
yDirection :: Plane3d (space @ units) defines -> Direction3d space
yDirection (Plane3d _ basis) = PlanarBasis3d.yDirection basis

{-| Get the X axis of a plane.

This is an axis formed from the plane's origin point and X direction.
-}
xAxis :: Plane3d (space @ units) defines -> Axis3d (space @ units)
xAxis plane = Axis3d (originPoint plane) (xDirection plane)

{-| Get the Y axis of a plane.

This is an axis formed from the plane's origin point and Y direction.
-}
yAxis :: Plane3d (space @ units) defines -> Axis3d (space @ units)
yAxis plane = Axis3d (originPoint plane) (yDirection plane)

{-| Move a plane so that its origin point is the given point.

The orientation of the plane will remain unchanged.
-}
moveTo :: Point3d (space @ units) -> Plane3d (space @ units) defines -> Plane3d (space @ units) defines
moveTo p0 (Plane3d _ basis) = Plane3d p0 basis

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Plane3d (local @ units) defines ->
  Plane3d (global @ units) defines
placeIn frame (Plane3d p0 basis) =
  Plane3d
    (Point3d.placeIn frame p0)
    (PlanarBasis3d.placeIn frame basis)

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Plane3d (global @ units) defines ->
  Plane3d (local @ units) defines
relativeTo frame (Plane3d p0 basis) =
  Plane3d
    (Point3d.relativeTo frame p0)
    (PlanarBasis3d.relativeTo frame basis)

-- | Offset a plane in its normal direction by the given distance.
offsetBy :: Qty units -> Plane3d (space @ units) defines -> Plane3d (space @ units) defines
offsetBy = translateInOwn normalDirection

-- | Reverse a plane's X direction, which also reverses the plane's normal direction.
flipX :: Plane3d (space @ units) defines -> Plane3d (space @ units) defines
flipX (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.flipX b)

-- | Reverse a plane's Y direction, which also reverses the plane's normal direction.
flipY :: Plane3d (space @ units) defines -> Plane3d (space @ units) defines
flipY (Plane3d p0 b) = Plane3d p0 (PlanarBasis3d.flipY b)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ units) ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
transformBy transform (Plane3d p0 basis) =
  Plane3d (Point3d.transformBy transform p0) (PlanarBasis3d.transformBy transform basis)

translateBy ::
  Vector3d (space @ units) ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Qty units ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d (space @ units) ->
  Qty units ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d (space @ units) ->
  Angle ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
rotateAround = Transform3d.rotateAroundImpl transformBy

mirrorAcross ::
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
mirrorAcross = Transform3d.mirrorAcrossImpl transformBy

translateByOwn ::
  (Plane3d (space @ units) defines -> Vector3d (space @ units)) ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn ::
  (Plane3d (space @ units) defines -> Direction3d space) ->
  Qty units ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Plane3d (space @ units) defines -> Axis3d (space @ units)) ->
  Qty units ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Plane3d (space @ units) defines -> Axis3d (space @ units)) ->
  Angle ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy

mirrorAcrossOwn ::
  (Plane3d (space @ units) defines -> Plane3d (space @ units) defines) ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
mirrorAcrossOwn = Transform3d.mirrorAcrossOwnImpl transformBy
