module Interval.Unsafe (Interval (..)) where

import OpenSolid

data Interval units = Interval (Quantity units) (Quantity units)
