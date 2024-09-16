module Range (Range, from) where

import OpenSolid

type role Range phantom

type Range :: Units -> Type
data Range units

from :: Qty units -> Qty units -> Range units
