module OpenSolid.Surface3D.Edge (Edge) where

import OpenSolid.Prelude

type role Edge nominal

data Edge (space :: Type)

instance Show (Edge space)
