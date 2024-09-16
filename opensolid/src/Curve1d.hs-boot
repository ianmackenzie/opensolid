module Curve1d
  ( Curve1d
  , pointOn
  , segmentBounds
  , derivative
  )
where

import OpenSolid
import Range (Range)

type role Curve1d nominal

type Curve1d :: Units -> Type
data Curve1d units

pointOn :: Curve1d units -> Float -> Qty units
segmentBounds :: Curve1d units -> Range Unitless -> Range units
derivative :: Curve1d units -> Curve1d units
