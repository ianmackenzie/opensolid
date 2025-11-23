module OpenSolid.UvBounds (UvBounds, unitSquare) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Prelude

type UvBounds = Bounds2d UvSpace Unitless

unitSquare :: UvBounds
unitSquare = Bounds2d Bounds.unitInterval Bounds.unitInterval
