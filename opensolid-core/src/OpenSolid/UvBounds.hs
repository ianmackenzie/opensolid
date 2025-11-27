module OpenSolid.UvBounds (UvBounds, unitSquare) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Prelude

type UvBounds = Bounds2d Unitless UvSpace

unitSquare :: UvBounds
unitSquare = Bounds2d Bounds.unitInterval Bounds.unitInterval
