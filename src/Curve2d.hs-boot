module Curve2d (
    Curve2d (Curve2d),
    IsCurve2d,
) where

import Data.Kind (Type)

class IsCurve2d curve

type Curve2d :: Type -> Type
data Curve2d coordinates = forall curve. IsCurve2d curve => Curve2d (curve coordinates)
