module Quantity (
    Quantity,
    zero,
    baseUnit,
    baseUnits,
    inBaseUnits,
    sqrt,
) where

import OpenSolid

baseUnits :: Float -> Quantity units
baseUnits (Quantity value) = Quantity value

inBaseUnits :: Quantity units -> Float
inBaseUnits (Quantity value) = Quantity value

zero :: Quantity units
zero = baseUnits 0.0

baseUnit :: Quantity units
baseUnit = baseUnits 1.0
