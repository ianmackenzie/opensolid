module OpenSolid.Curve2d.MedialAxis (Segment (..)) where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

data Segment space units = Segment
  { t1 :: Curve Unitless
  , t2 :: Curve Unitless
  , t12 :: Curve2d UvSpace Unitless
  , curve :: Curve2d space units
  , radius :: Curve units
  }
