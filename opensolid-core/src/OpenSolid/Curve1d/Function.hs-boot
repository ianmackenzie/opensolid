module OpenSolid.Curve1d.Function
  ( Function
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role Function nominal

type Function :: Type -> Type
data Function units

evaluate :: Function units -> Float -> Qty units
evaluateBounds :: Function units -> Range Unitless -> Range units
derivative :: Function units -> Function units
