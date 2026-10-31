module OpenSolid.Surface3D.Edge (Edge) where

import OpenSolid.Prelude
import OpenSolid.Space qualified as Space

type role Edge nominal

data Edge (space :: Type)

instance Show (Edge space)

instance Space.Coercion (Edge space1) (Edge space2)
