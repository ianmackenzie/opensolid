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
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d, Plane3d (Plane3d), Transform3d)
import OpenSolid.Transform qualified as Transform

through :: Point3d (space @ units) -> Direction3d space -> Plane3d (space @ units) defines
through p0 n = Plane3d p0 (Basis3d.fromZDirection n)

xy :: Plane3d (space @ units) (Defines local)
xy = Plane3d Point3d.origin Basis3d.xyz

yx :: Plane3d (space @ units) (Defines local)
yx = Plane3d Point3d.origin Basis3d.yxNegativeZ

zx :: Plane3d (space @ units) (Defines local)
zx = Plane3d Point3d.origin Basis3d.zxy

xz :: Plane3d (space @ units) (Defines local)
xz = Plane3d Point3d.origin Basis3d.xzNegativeY

yz :: Plane3d (space @ units) (Defines local)
yz = Plane3d Point3d.origin Basis3d.yzx

zy :: Plane3d (space @ units) (Defines local)
zy = Plane3d Point3d.origin Basis3d.zyNegativeX

originPoint :: Plane3d (space @ units) defines -> Point3d (space @ units)
originPoint (Plane3d p0 _) = p0

normalDirection :: Plane3d (space @ units) defines -> Direction3d space
normalDirection (Plane3d _ basis) = Basis3d.zDirection basis

xDirection :: Plane3d (space @ units) defines -> Direction3d space
xDirection (Plane3d _ basis) = Basis3d.xDirection basis

yDirection :: Plane3d (space @ units) defines -> Direction3d space
yDirection (Plane3d _ basis) = Basis3d.yDirection basis

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Plane3d (local @ units) defines ->
  Plane3d (global @ units) defines
placeIn frame (Plane3d p0 basis) =
  Plane3d
    (Point3d.placeIn frame p0)
    (Basis3d.placeIn frame basis)

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Plane3d (global @ units) defines ->
  Plane3d (local @ units) defines
relativeTo frame (Plane3d p0 basis) =
  Plane3d
    (Point3d.relativeTo frame p0)
    (Basis3d.relativeTo frame basis)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ units) ->
  Plane3d (space @ units) defines ->
  Plane3d (space @ units) defines
transformBy transform (Plane3d p0 basis) =
  Plane3d
    (Point3d.transformBy transform p0)
    (Basis3d.transformBy transform basis)
