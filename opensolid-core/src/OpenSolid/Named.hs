module OpenSolid.Named ((:::) (Named)) where

import Data.Kind (Type)
import GHC.OverloadedLabels qualified
import GHC.TypeLits (Symbol)
import Prelude (type (~))

type (:::) :: Symbol -> Type -> Type
newtype name ::: a = Named a

infix 0 :::

instance
  (name1 ~ name2, a1 ~ a2) =>
  GHC.OverloadedLabels.IsLabel name1 (a1 -> name2 ::: a2)
  where
  fromLabel = Named
