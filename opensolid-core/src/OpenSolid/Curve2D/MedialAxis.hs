module OpenSolid.Curve2D.MedialAxis (Segment (..)) where

import OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import OpenSolid.UvSpace (UvSpace)

data Segment units space = Segment
  { t1 :: Curve1D Unitless
  , t2 :: Curve1D Unitless
  , t12 :: Curve2D Unitless UvSpace
  , curve :: Curve2D units space
  , radius :: Curve1D units
  }
