module Curve1d (Curve1d) where

import Data.Kind (Type)

type role Curve1d nominal

type Curve1d :: Type -> Type
data Curve1d units
