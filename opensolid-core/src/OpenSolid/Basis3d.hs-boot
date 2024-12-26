module OpenSolid.Basis3d
  ( Basis3d
  , xDirection
  , yDirection
  , zDirection
  )
where

import {-# SOURCE #-} OpenSolid.Direction3d (Direction3d)
import OpenSolid.Prelude

type role Basis3d nominal nominal

type Basis3d :: Type -> LocalSpace -> Type
data Basis3d space defines where
  Basis3d ::
    { xDirection :: Direction3d space
    , yDirection :: Direction3d space
    , zDirection :: Direction3d space
    } ->
    Basis3d space defines
