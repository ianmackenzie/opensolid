module Units (Coercion (..)) where

import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified

class Coercion a b | a -> b where
    drop :: a -> b
    drop = unsafeCoerce

    add :: b -> a
    add = unsafeCoerce

instance Coercion Prelude.Double Prelude.Double
