module Quantity (
    Quantity,
    zero,
    baseUnit,
    baseUnits,
    inBaseUnits,
    unwrap,
    sqrt,
) where

import OpenSolid

baseUnits :: Float -> Quantity units
baseUnits (Quantity value) =
    Quantity value

inBaseUnits :: Quantity units -> Float
inBaseUnits (Quantity value) =
    Quantity value

unwrap :: Quantity units -> Float
unwrap =
    inBaseUnits

zero :: Quantity units
zero =
    baseUnits 0.0

baseUnit :: Quantity units
baseUnit =
    baseUnits 1.0
