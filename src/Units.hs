module Units
  ( Coercion
  , drop
  , add
  , wrap
  , unwrap
  )
where

import OpenSolid
import Unsafe.Coerce (unsafeCoerce)

class Coercion (a :: Type -> Type)

instance Coercion Qty

data Units units

data Square units

instance IsProduct (Qty (Units units)) (Qty (Units units)) (Qty (Square units))

instance IsQuotient (Qty (Square units)) (Qty (Units units)) (Qty (Units units))

instance Squared (Qty (Units units)) (Qty (Square units))

drop :: Coercion a => a units -> a Unitless
drop = unsafeCoerce

add :: Coercion a => a Unitless -> a units
add = unsafeCoerce

wrap :: Coercion a => a units -> a (Units units)
wrap = unsafeCoerce

unwrap :: Coercion a => a (Units units) -> a units
unwrap = unsafeCoerce
