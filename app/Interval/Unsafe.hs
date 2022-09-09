module Interval.Unsafe (Interval (..)) where

import OpenSolid

data Interval units = Interval (Quantity units) (Quantity units)
    deriving (Eq)

deriving instance Show (Quantity units) => Show (Interval units)
