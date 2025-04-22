module OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment, t1, t2, sign)) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

data OverlappingSegment = OverlappingSegment
  { t1 :: Bounds Unitless
  , t2 :: Bounds Unitless
  , sign :: Sign
  }
  deriving (Eq, Show)
