module OpenSolid.Continuity (Continuity (..)) where

import OpenSolid.Prelude

data Continuity = Crossing | Tangent Sign | Indistinguishable Sign deriving (Eq, Ord, Show)
