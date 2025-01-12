module OpenSolid.Curve
  ( Curve
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role Curve nominal

type Curve :: Type -> Type
data Curve units

evaluate :: Curve units -> Float -> Qty units
evaluateBounds :: Curve units -> Range Unitless -> Range units
derivative :: Curve units -> Curve units
