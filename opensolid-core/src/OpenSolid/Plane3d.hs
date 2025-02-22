module OpenSolid.Plane3d
  ( Plane3d (Plane3d)
  , through
  , xy
  , yx
  , zx
  , xz
  , yz
  , zy
  , originPoint
  , normalDirection
  , normalAxis
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
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

-- | Get the X direction of a plane.
xDirection :: Plane3d (space @ units) defines -> Direction3d space
xDirection (Plane3d _ basis) = PlanarBasis3d.xDirection basis

-- | Get the Y direction of a plane.
yDirection :: Plane3d (space @ units) defines -> Direction3d space
yDirection (Plane3d _ basis) = PlanarBasis3d.yDirection basis

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
