module OpenSolid.Range
  ( Range
  , unit
  , lowerBound
  , upperBound
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Qty (Qty)
import OpenSolid.Unitless (Unitless)

type role Range phantom

type Range :: Type -> Type
data Range units

unit :: Range Unitless
lowerBound :: Range units -> Qty units
upperBound :: Range units -> Qty units
