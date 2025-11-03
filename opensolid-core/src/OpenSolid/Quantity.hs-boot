module OpenSolid.Quantity (Quantity (Quantity)) where

import Data.Kind (Type)
import OpenSolid.Unitless (Unitless)
import Prelude (Double)
import Prelude qualified

type role Quantity phantom

type Quantity :: Type -> Type
newtype Quantity units = Quantity Double

instance Prelude.Num (Quantity Unitless)

instance Prelude.Real (Quantity Unitless)

instance Prelude.Fractional (Quantity Unitless)

instance Prelude.RealFrac (Quantity Unitless)

instance Prelude.Floating (Quantity Unitless)

instance Prelude.RealFloat (Quantity Unitless)
