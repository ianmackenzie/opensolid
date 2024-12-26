module Basis2d
  ( Basis2d
  , xDirection
  , yDirection
  )
where

import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid.Prelude

type role Basis2d nominal nominal

type Basis2d :: Type -> LocalSpace -> Type
data Basis2d space defines where
  Basis2d ::
    { xDirection :: Direction2d space
    , yDirection :: Direction2d space
    } ->
    Basis2d space defines
