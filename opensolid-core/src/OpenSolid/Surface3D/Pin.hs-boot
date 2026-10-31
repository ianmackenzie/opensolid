module OpenSolid.Surface3D.Pin (Pin) where

import OpenSolid.Prelude
import OpenSolid.Space qualified as Space

type role Pin phantom

data Pin (space :: Type)

instance Show (Pin space)

instance Space.Coercion (Pin space1) (Pin space2)
