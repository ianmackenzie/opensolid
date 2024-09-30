module Jit.Expression (Expression) where

import Data.Kind (Type)

type Expression :: Type -> Type

type role Expression nominal

data Expression a
