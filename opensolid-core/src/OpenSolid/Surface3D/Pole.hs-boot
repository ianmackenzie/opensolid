module OpenSolid.Surface3D.Pole (Pole) where

import OpenSolid.Prelude

type role Pole phantom

data Pole (space :: Type)

instance Show (Pole space)
