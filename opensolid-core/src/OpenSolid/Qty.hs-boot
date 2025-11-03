module OpenSolid.Qty (Qty (Qty)) where

import Data.Kind (Type)
import OpenSolid.Unitless (Unitless)
import Prelude (Double)
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty Double

instance Prelude.Num (Qty Unitless)

instance Prelude.Real (Qty Unitless)

instance Prelude.Fractional (Qty Unitless)

instance Prelude.RealFrac (Qty Unitless)

instance Prelude.Floating (Qty Unitless)

instance Prelude.RealFloat (Qty Unitless)
