module OpenSolid.UvBag (UvBag) where

import OpenSolid.Bag (Bag)
import OpenSolid.UvBounds (UvBounds)

type UvBag item = Bag UvBounds item
