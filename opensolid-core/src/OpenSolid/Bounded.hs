module OpenSolid.Bounded (Bounded (..)) where

import Data.Kind (Constraint)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

type Bounded :: Type -> Type -> Constraint
class Bounds b => Bounded a b | a -> b where
  bounds :: a -> b
