module Qty (Qty (Qty)) where

import Data.Kind (Type)
import {-# SOURCE #-} Units (Units (..))
import Prelude qualified

type role Qty phantom

type Qty :: Units -> Type
newtype Qty units = Qty Prelude.Double

instance Prelude.Num (Qty Unitless)

instance Prelude.Real (Qty Unitless)

instance Prelude.Fractional (Qty Unitless)

instance Prelude.RealFrac (Qty Unitless)

instance Prelude.Floating (Qty Unitless)

instance Prelude.RealFloat (Qty Unitless)
