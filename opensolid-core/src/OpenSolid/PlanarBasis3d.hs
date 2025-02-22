module OpenSolid.PlanarBasis3d
  ( PlanarBasis3d
  , coerce
  , xy
  , yx
  , yz
  , zy
  , zx
  , xz
  , fromXDirection
  , fromYDirection
  , fromNormalDirection
  , orthonormalize
  , flipX
  , flipY
  , xDirection
  , yDirection
  , normalDirection
  , transformBy
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Basis3d
  , Direction3d (Unit3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Transform3d
  , Vector3d
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3d qualified as Vector3d

coerce :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
coerce (PlanarBasis3d i j) = PlanarBasis3d i j

xy :: PlanarBasis3d space defines
xy = PlanarBasis3d Direction3d.x Direction3d.y

yx :: PlanarBasis3d space defines
yx = PlanarBasis3d Direction3d.y Direction3d.x

yz :: PlanarBasis3d space defines
yz = PlanarBasis3d Direction3d.y Direction3d.z

zy :: PlanarBasis3d space defines
zy = PlanarBasis3d Direction3d.z Direction3d.y

zx :: PlanarBasis3d space defines
zx = PlanarBasis3d Direction3d.z Direction3d.x

xz :: PlanarBasis3d space defines
xz = PlanarBasis3d Direction3d.x Direction3d.z

fromXDirection :: Direction3d space -> PlanarBasis3d space defines
fromXDirection dx = PlanarBasis3d dx (Direction3d.perpendicularTo dx)

fromYDirection :: Direction3d space -> PlanarBasis3d space defines
fromYDirection dy = PlanarBasis3d (Direction3d.perpendicularTo dy) dy

fromNormalDirection :: Direction3d space -> PlanarBasis3d space defines
fromNormalDirection direction = do
  let dx = Direction3d.perpendicularTo direction
  let dy = Unit3d (direction >< dx)
  PlanarBasis3d dx dy

orthonormalize ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Maybe (PlanarBasis3d space defines)
orthonormalize vx vxy =
  case Vector3d.direction vx of
    Failure Vector3d.IsZero -> Nothing
    Success dx -> do
      let vy = vxy - Vector3d.projectionIn dx vxy
      case Vector3d.direction vy of
        Failure Vector3d.IsZero -> Nothing
        Success dy -> Just (PlanarBasis3d dx dy)

flipX :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
flipX (PlanarBasis3d i j) = PlanarBasis3d -i j

flipY :: PlanarBasis3d space defines1 -> PlanarBasis3d space defines2
flipY (PlanarBasis3d i j) = PlanarBasis3d i -j

xDirection :: PlanarBasis3d space defines -> Direction3d space
xDirection (PlanarBasis3d i _) = i

yDirection :: PlanarBasis3d space defines -> Direction3d space
yDirection (PlanarBasis3d _ j) = j

normalDirection :: PlanarBasis3d space defines -> Direction3d space
normalDirection (PlanarBasis3d i j) = Unit3d (i >< j)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ translationUnits) ->
  PlanarBasis3d space defines ->
  PlanarBasis3d space defines
transformBy transform (PlanarBasis3d i j) =
  PlanarBasis3d (Direction3d.transformBy transform i) (Direction3d.transformBy transform j)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  PlanarBasis3d local defines ->
  PlanarBasis3d global defines
placeIn (Frame3d _ basis) = placeInBasis basis

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  PlanarBasis3d global defines ->
  PlanarBasis3d local defines
relativeTo (Frame3d _ basis) = relativeToBasis basis

placeInBasis ::
  Basis3d global (Defines local) ->
  PlanarBasis3d local defines ->
  PlanarBasis3d global defines
placeInBasis globalBasis (PlanarBasis3d i j) =
  PlanarBasis3d
    (Direction3d.placeInBasis globalBasis i)
    (Direction3d.placeInBasis globalBasis j)

relativeToBasis ::
  Basis3d global (Defines local) ->
  PlanarBasis3d global defines ->
  PlanarBasis3d local defines
relativeToBasis globalBasis (PlanarBasis3d i j) =
  PlanarBasis3d
    (Direction3d.relativeToBasis globalBasis i)
    (Direction3d.relativeToBasis globalBasis j)
