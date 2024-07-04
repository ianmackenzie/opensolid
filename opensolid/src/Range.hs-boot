module Range (Range, from) where

import OpenSolid

type role Range phantom

type Range :: Type -> Type
data Range units

from :: Qty units -> Qty units -> Range units
