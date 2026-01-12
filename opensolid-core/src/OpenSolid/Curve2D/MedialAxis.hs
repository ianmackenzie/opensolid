module OpenSolid.Curve2D.MedialAxis (Segment (..)) where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude

data Segment units space = Segment
  { t1 :: Curve Unitless
  , t2 :: Curve Unitless
  , t12 :: Curve2D Unitless UvSpace
  , curve :: Curve2D units space
  , radius :: Curve units
  }
