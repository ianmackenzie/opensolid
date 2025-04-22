module OpenSolid.Bounds
  ( Bounds
  , unitInterval
  , lower
  , upper
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Qty (Qty)
import OpenSolid.Unitless (Unitless)

type role Bounds phantom

type Bounds :: Type -> Type
data Bounds units

unitInterval :: Bounds Unitless
lower :: Bounds units -> Qty units
upper :: Bounds units -> Qty units
