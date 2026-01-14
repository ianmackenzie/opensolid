module OpenSolid.Interval
  ( Interval
  , unit
  , lower
  , upper
  )
where

import Data.Kind (Type)
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)

type role Interval phantom

type Interval :: Type -> Type
data Interval units

unit :: Interval Unitless
lower :: Interval units -> Quantity units
upper :: Interval units -> Quantity units
