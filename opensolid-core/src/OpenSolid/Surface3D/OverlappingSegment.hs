module OpenSolid.Surface3D.OverlappingSegment (OverlappingSegment (..)) where

import OpenSolid.Prelude
import OpenSolid.UvRegion (UvRegion)

data OverlappingSegment = OverlappingSegment
  { uvRegion1 :: UvRegion
  , uvRegion2 :: UvRegion
  , alignment :: Sign
  }
