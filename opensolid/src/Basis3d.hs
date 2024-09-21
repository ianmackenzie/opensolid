module Basis3d
  ( Basis3d
  , coerce
  , xyz
  , yzx
  , zxy
  , yxNegativeZ
  , zyNegativeX
  , xzNegativeY
  , flipX
  , flipY
  , flipZ
  , fromXDirection
  , fromYDirection
  , fromZDirection
  , xDirection
  , yDirection
  , zDirection
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , inverse
  )
where

import Direction3d (Direction3d)
import Direction3d qualified
import Float qualified
import {-# SOURCE #-} Frame3d (Frame3d)
import {-# SOURCE #-} Frame3d qualified
import OpenSolid
import Vector3d qualified

type role Basis3d nominal nominal

type Basis3d :: Type -> LocalSpace -> Type
data Basis3d space defines where
  Basis3d ::
    { xDirection :: Direction3d space
    , yDirection :: Direction3d space
    , zDirection :: Direction3d space
    } ->
    Basis3d space defines

deriving instance Eq (Basis3d space defines)

deriving instance Show (Basis3d space defines)

coerce :: Basis3d space defines1 -> Basis3d space defines2
coerce Basis3d{xDirection, yDirection, zDirection} = Basis3d{xDirection, yDirection, zDirection}

xyz :: Basis3d space defines
xyz = Basis3d Direction3d.x Direction3d.y Direction3d.z

yzx :: Basis3d space defines
yzx = Basis3d Direction3d.y Direction3d.z Direction3d.x

zxy :: Basis3d space defines
zxy = Basis3d Direction3d.z Direction3d.x Direction3d.y

yxNegativeZ :: Basis3d space defines
yxNegativeZ = Basis3d Direction3d.y Direction3d.x Direction3d.negativeZ

zyNegativeX :: Basis3d space defines
zyNegativeX = Basis3d Direction3d.z Direction3d.y Direction3d.negativeX

xzNegativeY :: Basis3d space defines
xzNegativeY = Basis3d Direction3d.x Direction3d.z Direction3d.negativeY

flipX :: Basis3d space defines -> Basis3d space defines
flipX (Basis3d dx dy dz) = Basis3d -dx dy dz

flipY :: Basis3d space defines -> Basis3d space defines
flipY (Basis3d dx dy dz) = Basis3d dx -dy dz

flipZ :: Basis3d space defines -> Basis3d space defines
flipZ (Basis3d dx dy dz) = Basis3d dx dy -dz

perpendicularDirections :: Direction3d space -> (Direction3d space, Direction3d space)
perpendicularDirections direction = do
  let (dx, dy, dz) = Direction3d.components direction
  let absX = Float.abs dx
  let absY = Float.abs dy
  let absZ = Float.abs dz
  let v1 =
        if
          | absX <= absY && absX <= absZ -> do
              let scale = Float.hypot2 dy dz
              Vector3d.xyz 0.0 (-dz / scale) (dy / scale)
          | absY <= absX && absY <= absZ -> do
              let scale = Float.hypot2 dx dz
              Vector3d.xyz (dz / scale) 0.0 (-dx / scale)
          | otherwise -> do
              let scale = Float.hypot2 dx dy
              Vector3d.xyz (-dy / scale) (dx / scale) 0.0
  let v2 = direction >< v1
  (Direction3d.unsafe v1, Direction3d.unsafe v2)

fromXDirection :: Direction3d space -> Basis3d space defines
fromXDirection dx = do
  let (dy, dz) = perpendicularDirections dx
  Basis3d dx dy dz

fromYDirection :: Direction3d space -> Basis3d space defines
fromYDirection dy = do
  let (dz, dx) = perpendicularDirections dy
  Basis3d dx dy dz

fromZDirection :: Direction3d space -> Basis3d space defines
fromZDirection dz = do
  let (dx, dy) = perpendicularDirections dz
  Basis3d dx dy dz

placeIn ::
  Frame3d (global @ units) (Defines space) ->
  Basis3d space (Defines local) ->
  Basis3d global (Defines local)
placeIn frame = placeInBasis (Frame3d.basis frame)

relativeTo ::
  Frame3d (global @ units) (Defines space) ->
  Basis3d global (Defines local) ->
  Basis3d space (Defines local)
relativeTo frame = relativeToBasis (Frame3d.basis frame)

placeInBasis ::
  Basis3d global (Defines space) ->
  Basis3d space (Defines local) ->
  Basis3d global (Defines local)
placeInBasis globalBasis basis =
  Basis3d
    { xDirection = Direction3d.placeInBasis globalBasis (xDirection basis)
    , yDirection = Direction3d.placeInBasis globalBasis (yDirection basis)
    , zDirection = Direction3d.placeInBasis globalBasis (zDirection basis)
    }

relativeToBasis ::
  Basis3d global (Defines space) ->
  Basis3d global (Defines local) ->
  Basis3d space (Defines local)
relativeToBasis globalBasis basis =
  Basis3d
    { xDirection = Direction3d.relativeToBasis globalBasis (xDirection basis)
    , yDirection = Direction3d.relativeToBasis globalBasis (yDirection basis)
    , zDirection = Direction3d.relativeToBasis globalBasis (zDirection basis)
    }

inverse :: Basis3d global (Defines local) -> Basis3d local (Defines global)
inverse basis = xyz |> relativeToBasis basis
