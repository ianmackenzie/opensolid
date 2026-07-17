module OpenSolid.Bag (Bag (Empty, Full), fromMaybeSet, toMaybeSet) where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Set (Set)

data Bag b a = Empty | Full (Set b a)

fromMaybeSet :: Maybe (Set b a) -> Bag b a
toMaybeSet :: Bag b a -> Maybe (Set b a)
