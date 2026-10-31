module OpenSolid.Surface3D.Pin (Pin) where

import OpenSolid.Prelude

type role Pin phantom

data Pin (space :: Type)

instance Show (Pin space)
