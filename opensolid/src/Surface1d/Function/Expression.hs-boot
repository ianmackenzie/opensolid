module Surface1d.Function.Expression (Expression) where

import Data.Kind (Type)

type role Expression nominal

type Expression :: Type -> Type
data Expression units
