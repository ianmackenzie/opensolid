module OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment (..)) where

import OpenSolid.Interval (Interval)
import OpenSolid.Prelude

data OverlappingSegment = OverlappingSegment
  { tBounds1 :: Interval Unitless
  , tBounds2 :: Interval Unitless
  , alignment :: Sign
  }
  deriving (Eq, Show)
