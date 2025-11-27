module OpenSolid.Curve2d.MedialAxis (Segment (..)) where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

data Segment units space = Segment
  { t1 :: Curve Unitless
  , t2 :: Curve Unitless
  , t12 :: Curve2d Unitless UvSpace
  , curve :: Curve2d units space
  , radius :: Curve units
  }
