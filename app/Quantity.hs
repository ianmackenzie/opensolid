module Quantity (
    Quantity,
    baseUnit,
    baseUnits,
    inBaseUnits,
) where

import OpenSolid

baseUnits :: Float -> Quantity units
baseUnits (Quantity value) =
    Quantity value

inBaseUnits :: Quantity units -> Float
inBaseUnits (Quantity value) =
    Quantity value

baseUnit :: Quantity units
baseUnit =
    baseUnits 1.0
