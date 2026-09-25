module OpenSolid.UvCurve
  ( UvCurve
  , IsDegenerate
  , new
  , unsafe
  )
where

import {-# SOURCE #-} OpenSolid.Curve (UvCurve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type IsDegenerate = Curve.IsDegenerate 2 Unitless Void

new :: Curve2D.Compiled Unitless -> VectorCurve2D Unitless -> Result IsDegenerate UvCurve
unsafe :: Curve2D.Compiled Unitless -> VectorCurve2D Unitless -> UvCurve
