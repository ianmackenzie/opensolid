module OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment, t1, t2, sign)) where

import OpenSolid.Prelude
import OpenSolid.Range (Range)

data OverlappingSegment = OverlappingSegment
  { t1 :: Range Unitless
  , t2 :: Range Unitless
  , sign :: Sign
  }
  deriving (Eq, Show)
