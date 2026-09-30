module OpenSolid.UvSet (UvSet) where

import OpenSolid.Set (Set)
import OpenSolid.UvBounds (UvBounds)

type UvSet item = Set UvBounds item
