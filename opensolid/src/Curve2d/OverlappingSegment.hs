module Curve2d.OverlappingSegment (OverlappingSegment (OverlappingSegment, t1, t2, sign)) where

import OpenSolid
import Range (Range)

data OverlappingSegment = OverlappingSegment
  { t1 :: Range Unitless
  , t2 :: Range Unitless
  , sign :: Sign
  }
  deriving (Eq, Show)
