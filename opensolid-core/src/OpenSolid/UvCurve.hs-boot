module OpenSolid.UvCurve (UvCurve, new) where

import {-# SOURCE #-} OpenSolid.Curve (UvCurve)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

new :: Curve2D.Compiled Unitless -> VectorCurve2D Unitless -> UvCurve
