module OpenSolid.Continuity (Continuity (..)) where

import OpenSolid.Prelude

data Continuity = G0 | G1 Sign | G2 Sign deriving (Eq, Ord, Show)
