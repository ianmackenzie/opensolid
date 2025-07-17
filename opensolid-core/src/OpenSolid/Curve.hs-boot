module OpenSolid.Curve
  ( Curve
  , Compiled
  , compiled
  , derivative
  , evaluate
  , evaluateBounds
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Functions (Curve (..), CurveCompiled)
import OpenSolid.Prelude

type Compiled units = CurveCompiled units

compiled :: Curve units -> Compiled units
derivative :: Curve units -> Curve units
evaluate :: Curve units -> Float -> Qty units
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
