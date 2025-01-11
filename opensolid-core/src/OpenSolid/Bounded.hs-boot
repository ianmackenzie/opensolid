module OpenSolid.Bounded (Bounded) where

import Data.Kind (Constraint)
import OpenSolid.Prelude

type Bounded :: Type -> Type -> Constraint
class Bounded a b | a -> b
