module OpenSolid.Quantity (Quantity (Quantity)) where

import Data.Kind (Type)
import OpenSolid.Unitless (Unitless)
import Prelude (Double, type (~))
import Prelude qualified

type role Quantity phantom

type Quantity :: Type -> Type
newtype Quantity units = Quantity Double

instance units ~ Unitless => Prelude.Num (Quantity units)

instance units ~ Unitless => Prelude.Real (Quantity units)

instance units ~ Unitless => Prelude.Fractional (Quantity units)

instance units ~ Unitless => Prelude.RealFrac (Quantity units)

instance units ~ Unitless => Prelude.Floating (Quantity units)

instance units ~ Unitless => Prelude.RealFloat (Quantity units)
