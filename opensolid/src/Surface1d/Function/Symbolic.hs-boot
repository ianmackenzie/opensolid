module Surface1d.Function.Symbolic (Symbolic) where

import Data.Kind (Type)

type role Symbolic nominal

type Symbolic :: Type -> Type
data Symbolic units
