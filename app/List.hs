module List (map) where

import OpenSolid
import qualified Prelude

map :: (a -> b) -> List a -> List b
map =
    Prelude.fmap
