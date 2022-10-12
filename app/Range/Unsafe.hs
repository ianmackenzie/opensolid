module Range.Unsafe (Range (..)) where

import OpenSolid

data Range units = Range !(Quantity units) !(Quantity units)
    deriving (Eq)

deriving instance Show (Quantity units) => Show (Range units)
