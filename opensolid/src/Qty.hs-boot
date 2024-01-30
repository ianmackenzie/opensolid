module Qty (Qty (Qty)) where

import Data.Kind (Type)
import {-# SOURCE #-} Units (Unitless)
import Prelude qualified

type role Qty nominal

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double

instance Prelude.Floating (Qty Unitless)
