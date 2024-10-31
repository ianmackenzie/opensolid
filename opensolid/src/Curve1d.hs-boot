module Curve1d
  ( Curve1d
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid
import Range (Range)

type role Curve1d nominal

type Curve1d :: Type -> Type
data Curve1d units

evaluate :: Curve1d units -> Float -> Qty units
evaluateBounds :: Curve1d units -> Range Unitless -> Range units
derivative :: Curve1d units -> Curve1d units
