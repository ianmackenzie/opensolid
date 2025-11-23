module OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (..)) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

data OverlappingSegment = OverlappingSegment
  { tBounds1 :: Bounds Unitless
  , tBounds2 :: Bounds Unitless
  , alignment :: Sign
  }
  deriving (Eq, Show)
