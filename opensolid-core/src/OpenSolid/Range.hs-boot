module OpenSolid.Range
  ( Range
  , from
  , lowerBound
  , upperBound
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Qty (Qty)

type role Range phantom

type Range :: Type -> Type
data Range units

from :: Qty units -> Qty units -> Range units
lowerBound :: Range units -> Qty units
upperBound :: Range units -> Qty units
