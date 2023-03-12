module Qty (Qty(Qty)) where

import Data.Kind (Type)
import Prelude qualified

type role Qty nominal

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double
