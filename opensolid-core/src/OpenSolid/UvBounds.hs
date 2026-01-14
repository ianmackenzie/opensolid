module OpenSolid.UvBounds (UvBounds, unitSquare) where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude

type UvBounds = Bounds2D Unitless UvSpace

unitSquare :: UvBounds
unitSquare = Bounds2D Interval.unit Interval.unit
