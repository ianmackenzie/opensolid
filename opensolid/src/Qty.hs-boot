module Qty (Qty (Qty)) where

import Data.Kind (Type)
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double
