module OpenSolid.Bounds
  ( Bounds
  , unitInterval
  , lower
  , upper
  )
where

import Data.Kind (Type)
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)

type role Bounds phantom

type Bounds :: Type -> Type
data Bounds units

unitInterval :: Bounds Unitless
lower :: Bounds units -> Quantity units
upper :: Bounds units -> Quantity units
