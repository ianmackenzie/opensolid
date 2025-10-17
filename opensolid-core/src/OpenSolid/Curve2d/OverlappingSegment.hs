module OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment)) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

data OverlappingSegment = OverlappingSegment (Bounds Unitless) (Bounds Unitless) Sign
  deriving (Eq, Show)
