module OpenSolid.Surface3D.Pole (Pole) where

import OpenSolid.Prelude
import OpenSolid.Space qualified as Space

type role Pole phantom

data Pole (space :: Type)

instance Show (Pole space)

instance Space.Coercion (Pole space1) (Pole space2)
