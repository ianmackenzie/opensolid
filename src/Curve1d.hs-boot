module Curve1d
  ( Curve1d
  , evaluateAt
  , segmentBounds
  , derivative
  )
where

import Domain (Domain)
import OpenSolid
import Range (Range)

type role Curve1d nominal

type Curve1d :: Type -> Type
data Curve1d units

evaluateAt :: Float -> Curve1d units -> Qty units
segmentBounds :: Domain -> Curve1d units -> Range units
derivative :: Curve1d units -> Curve1d units
