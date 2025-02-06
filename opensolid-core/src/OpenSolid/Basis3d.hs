module OpenSolid.Basis3d
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

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Primitives (Basis3d (Basis3d), Frame3d (Frame3d))
import OpenSolid.Vector3d qualified as Vector3d

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
placeIn (Frame3d _ basis) = placeInBasis basis

relativeTo ::
  Frame3d (global @ units) (Defines space) ->
  Basis3d global (Defines local) ->
  Basis3d space (Defines local)
relativeTo (Frame3d _ basis) = relativeToBasis basis

placeInBasis ::
  Basis3d global (Defines space) ->
  Basis3d space (Defines local) ->
  Basis3d global (Defines local)
placeInBasis globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.placeInBasis globalBasis i)
    (Direction3d.placeInBasis globalBasis j)
    (Direction3d.placeInBasis globalBasis k)

relativeToBasis ::
  Basis3d global (Defines space) ->
  Basis3d global (Defines local) ->
  Basis3d space (Defines local)
relativeToBasis globalBasis (Basis3d i j k) =
  Basis3d
    (Direction3d.relativeToBasis globalBasis i)
    (Direction3d.relativeToBasis globalBasis j)
    (Direction3d.relativeToBasis globalBasis k)

inverse :: Basis3d global (Defines local) -> Basis3d local (Defines global)
inverse basis = xyz |> relativeToBasis basis
