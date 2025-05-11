module OpenSolid.Frame3d
  ( Frame3d (Frame3d)
  , coerce
  , xyz
  , atPoint
  , originPoint
  , basis
  , xDirection
  , yDirection
  , zDirection
  , xAxis
  , yAxis
  , zAxis
  , xyPlane
  , yxPlane
  , zxPlane
  , xzPlane
  , yzPlane
  , zyPlane
  , fromXAxis
  , fromYAxis
  , fromZAxis
  , fromXyPlane
  , fromYxPlane
  , fromZxPlane
  , fromXzPlane
  , fromYzPlane
  , fromZyPlane
  , placeIn
  , relativeTo
  , inverse
  , moveTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis3d (Axis3d (Axis3d))
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Basis3d (Basis3d)
  , Direction3d (Unit3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Vector3d
  )
import OpenSolid.Transform3d qualified as Transform3d

originPoint :: Frame3d (space @ units) defines -> Point3d (space @ units)
originPoint (Frame3d p0 _) = p0

basis :: Frame3d (space @ units) defines -> Basis3d space defines
basis (Frame3d _ b) = b

coerce :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
coerce (Frame3d p0 b) = Frame3d p0 (Basis3d.coerce b)

xDirection :: Frame3d (space @ units) defines -> Direction3d space
xDirection frame = Basis3d.xDirection (basis frame)

yDirection :: Frame3d (space @ units) defines -> Direction3d space
yDirection frame = Basis3d.yDirection (basis frame)

zDirection :: Frame3d (space @ units) defines -> Direction3d space
zDirection frame = Basis3d.zDirection (basis frame)

xyz :: Frame3d (space @ units) defines
xyz = atPoint Point3d.origin

atPoint :: Point3d (space @ units) -> Frame3d (space @ units) defines
atPoint p0 = Frame3d p0 Basis3d.xyz

xAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
xAxis frame = Axis3d.through (originPoint frame) (xDirection frame)

yAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
yAxis frame = Axis3d.through (originPoint frame) (yDirection frame)

zAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
zAxis frame = Axis3d.through (originPoint frame) (zDirection frame)

xyPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
xyPlane (Frame3d p0 (Basis3d i j _)) = Plane3d p0 (PlanarBasis3d i j)

yxPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
yxPlane (Frame3d p0 (Basis3d i j _)) = Plane3d p0 (PlanarBasis3d j i)

zxPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
zxPlane (Frame3d p0 (Basis3d i _ k)) = Plane3d p0 (PlanarBasis3d k i)

xzPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
xzPlane (Frame3d p0 (Basis3d i _ k)) = Plane3d p0 (PlanarBasis3d i k)

yzPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
yzPlane (Frame3d p0 (Basis3d _ j k)) = Plane3d p0 (PlanarBasis3d j k)

zyPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
zyPlane (Frame3d p0 (Basis3d _ j k)) = Plane3d p0 (PlanarBasis3d k j)

fromXAxis :: Axis3d (space @ units) -> Frame3d (space @ units) defines
fromXAxis (Axis3d p0 d) = Frame3d p0 (Basis3d.fromXDirection d)

fromYAxis :: Axis3d (space @ units) -> Frame3d (space @ units) defines
fromYAxis (Axis3d p0 d) = Frame3d p0 (Basis3d.fromYDirection d)

fromZAxis :: Axis3d (space @ units) -> Frame3d (space @ units) defines
fromZAxis (Axis3d p0 d) = Frame3d p0 (Basis3d.fromZDirection d)

fromXyPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromXyPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d i j (Unit3d (i `cross` j)))

fromYxPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromYxPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d j i (Unit3d (j `cross` i)))

fromZxPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromZxPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d j (Unit3d (i `cross` j)) i)

fromXzPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromXzPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d i (Unit3d (j `cross` i)) j)

fromYzPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromYzPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d (Unit3d (i `cross` j)) i j)

fromZyPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromZyPlane (Plane3d p0 (PlanarBasis3d i j)) = Frame3d p0 (Basis3d (Unit3d (j `cross` i)) j i)

placeIn ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (space @ units) (Defines local) ->
  Frame3d (global @ units) (Defines local)
placeIn globalFrame frame =
  Frame3d
    (Point3d.placeIn globalFrame (originPoint frame))
    (Basis3d.placeIn (basis globalFrame) (basis frame))

relativeTo ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (global @ units) (Defines local) ->
  Frame3d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame3d
    (Point3d.relativeTo globalFrame (originPoint frame))
    (Basis3d.relativeTo (basis globalFrame) (basis frame))

inverse :: Frame3d (global @ units) (Defines local) -> Frame3d (local @ units) (Defines global)
inverse frame = xyz |> relativeTo frame

moveTo ::
  Point3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
moveTo newOriginPoint (Frame3d _ b) = Frame3d newOriginPoint (Basis3d.coerce b)

transformBy ::
  Transform3d.Rigid (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
transformBy transform (Frame3d p0 b) =
  Frame3d (Point3d.transformBy transform p0) (Basis3d.transformBy transform b)

translateBy ::
  Vector3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d (space @ units) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d (space @ units) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAround = Transform3d.rotateAroundImpl transformBy

translateByOwn ::
  (Frame3d (space @ units) defines1 -> Vector3d (space @ units)) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn ::
  (Frame3d (space @ units) defines1 -> Direction3d space) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy
