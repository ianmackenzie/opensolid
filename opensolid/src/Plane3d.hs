module Plane3d
  ( Plane3d
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
  )
where

import Basis3d (Basis3d)
import Basis3d qualified
import Direction3d (Direction3d)
import OpenSolid.Prelude
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Point3d (Point3d)

type Plane3d :: CoordinateSystem -> LocalSpace -> Type
data Plane3d coordinateSystem defines where
  Plane3d ::
    Point3d (space @ units) ->
    Basis3d space (Defines local) ->
    Plane3d (space @ units) (Defines local)

through :: Point3d (space @ units) -> Direction3d space -> Plane3d (space @ units) (Defines local)
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
