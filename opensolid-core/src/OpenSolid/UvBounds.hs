module OpenSolid.UvBounds (UvBounds, unitSquare) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Prelude

type UvBounds = Bounds2D Unitless UvSpace

unitSquare :: UvBounds
unitSquare = Bounds2D Bounds.unitInterval Bounds.unitInterval
