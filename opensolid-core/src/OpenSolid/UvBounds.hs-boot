module OpenSolid.UvBounds (UvBounds) where

import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D)
import OpenSolid.UvSpace (UvSpace)

type UvBounds = Bounds2D Unitless UvSpace
