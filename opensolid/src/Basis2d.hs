module Basis2d
  ( Basis2d
  , xy
  , fromXDirection
  , fromYDirection
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , inverse
  )
where

import Direction2d (Direction2d)
import Direction2d qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid

type role Basis2d nominal nominal

type Basis2d :: Type -> LocalSpace -> Type
data Basis2d space defines where
  Basis2d ::
    { xDirection :: Direction2d space
    , yDirection :: Direction2d space
    } ->
    Basis2d space defines

deriving instance Show (Basis2d space defines)

xy :: Basis2d space defines
xy = Basis2d Direction2d.x Direction2d.y

fromXDirection :: Direction2d space -> Basis2d space defines
fromXDirection dx = Basis2d dx (Direction2d.rotateLeft dx)

fromYDirection :: Direction2d space -> Basis2d space defines
fromYDirection dy = Basis2d (Direction2d.rotateRight dy) dy

placeIn ::
  Frame2d (global @ units) (Defines space) ->
  Basis2d space (Defines local) ->
  Basis2d global (Defines local)
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ units) (Defines space) ->
  Basis2d global (Defines local) ->
  Basis2d space (Defines local)
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines space) ->
  Basis2d space (Defines local) ->
  Basis2d global (Defines local)
placeInBasis globalBasis basis =
  Basis2d
    { xDirection = Direction2d.placeInBasis globalBasis (xDirection basis)
    , yDirection = Direction2d.placeInBasis globalBasis (yDirection basis)
    }

relativeToBasis ::
  Basis2d global (Defines space) ->
  Basis2d global (Defines local) ->
  Basis2d space (Defines local)
relativeToBasis globalBasis basis =
  Basis2d
    { xDirection = Direction2d.relativeToBasis globalBasis (xDirection basis)
    , yDirection = Direction2d.relativeToBasis globalBasis (yDirection basis)
    }

inverse :: Basis2d global (Defines local) -> Basis2d local (Defines global)
inverse basis = xy |> relativeToBasis basis
