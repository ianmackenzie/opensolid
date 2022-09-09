module Quantity (
    Quantity,
    baseUnit,
    baseUnits,
    inBaseUnits,
    interpolateFrom,
    midpoint,
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

interpolateFrom :: Quantity units -> Quantity units -> Float -> Quantity units
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Quantity units -> Quantity units -> Quantity units
midpoint a b =
    a + 0.5 * (b - a)
