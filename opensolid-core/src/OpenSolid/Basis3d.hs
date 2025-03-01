module OpenSolid.Basis3d
  ( Basis3d
  , coerce
  , xyz
  , yzx
  , zxy
  , fromXDirection
  , fromYDirection
  , fromZDirection
  , xDirection
  , yDirection
  , zDirection
  , transformBy
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , inverse
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Basis3d (Basis3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  )
import OpenSolid.Transform3d qualified as Transform3d

coerce :: Basis3d space defines1 -> Basis3d space defines2
coerce (Basis3d i j k) = Basis3d i j k

xDirection :: Basis3d space defines -> Direction3d space
xDirection (Basis3d i _ _) = i

yDirection :: Basis3d space defines -> Direction3d space
yDirection (Basis3d _ j _) = j

zDirection :: Basis3d space defines -> Direction3d space
zDirection (Basis3d _ _ k) = k

xyz :: Basis3d space defines
xyz = Basis3d Direction3d.x Direction3d.y Direction3d.z

yzx :: Basis3d space defines
yzx = Basis3d Direction3d.y Direction3d.z Direction3d.x

zxy :: Basis3d space defines
zxy = Basis3d Direction3d.z Direction3d.x Direction3d.y

fromXDirection :: Direction3d space -> Basis3d space defines
fromXDirection dx = do
  let PlanarBasis3d dy dz = PlanarBasis3d.fromNormalDirection dx
  Basis3d dx dy dz

fromYDirection :: Direction3d space -> Basis3d space defines
fromYDirection dy = do
  let PlanarBasis3d dz dx = PlanarBasis3d.fromNormalDirection dy
  Basis3d dx dy dz

fromZDirection :: Direction3d space -> Basis3d space defines
fromZDirection dz = do
  let PlanarBasis3d dx dy = PlanarBasis3d.fromNormalDirection dz
  Basis3d dx dy dz

transformBy ::
  Transform3d.Rigid (space @ translationUnits) ->
  Basis3d space defines ->
  Basis3d space defines
transformBy transform (Basis3d i j k) =
  Basis3d
    (Direction3d.transformBy transform i)
    (Direction3d.transformBy transform j)
    (Direction3d.transformBy transform k)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Basis3d local defines ->
  Basis3d global defines
placeIn (Frame3d _ basis) = placeInBasis basis

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Basis3d global defines ->
  Basis3d local defines
relativeTo (Frame3d _ basis) = relativeToBasis basis

placeInBasis ::
  Basis3d global (Defines local) ->
  Basis3d local defines ->
  Basis3d global defines
placeInBasis globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.placeInBasis globalBasis i)
    (Direction3d.placeInBasis globalBasis j)
    (Direction3d.placeInBasis globalBasis k)

relativeToBasis ::
  Basis3d global (Defines local) ->
  Basis3d global defines ->
  Basis3d local defines
relativeToBasis globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.relativeToBasis globalBasis i)
    (Direction3d.relativeToBasis globalBasis j)
    (Direction3d.relativeToBasis globalBasis k)

inverse :: Basis3d global (Defines local) -> Basis3d local (Defines global)
inverse basis = xyz |> relativeToBasis basis
