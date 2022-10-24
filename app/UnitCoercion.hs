module UnitCoercion (UnitCoercion (..)) where

import Unsafe.Coerce (unsafeCoerce)
import qualified Prelude

class UnitCoercion a b | a -> b where
    dropUnits :: a -> b
    dropUnits =
        unsafeCoerce

    addUnits :: b -> a
    addUnits =
        unsafeCoerce

instance UnitCoercion Prelude.Double Prelude.Double
