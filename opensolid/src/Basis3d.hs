module Basis3d
  ( Basis3d
  , xyz
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
import {-# SOURCE #-} Frame3d (Frame3d)
import {-# SOURCE #-} Frame3d qualified
import OpenSolid

type role Basis3d nominal nominal

type Basis3d :: Type -> LocalSpace -> Type
data Basis3d space defines where
  Basis3d ::
    { xDirection :: Direction3d space
    , yDirection :: Direction3d space
    , zDirection :: Direction3d space
    } ->
    Basis3d space defines

deriving instance Show (Basis3d space defines)

xyz :: Basis3d space defines
xyz = Basis3d Direction3d.x Direction3d.y Direction3d.z

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
