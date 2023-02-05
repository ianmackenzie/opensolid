module Units (Coercion (..)) where

import OpenSolid
import Unsafe.Coerce (unsafeCoerce)

class Coercion a where
  drop :: a units -> a Unitless
  drop = unsafeCoerce

  add :: a Unitless -> a units
  add = unsafeCoerce

instance Coercion Qty
