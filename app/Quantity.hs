module Quantity (
    Quantity,
    zero,
    baseUnit,
    baseUnits,
    inBaseUnits,
    interpolateFrom,
    midpoint,
    hypot2,
    hypot3,
) where

import Data.Coerce (coerce)
import OpenSolid
import qualified Prelude

zero :: Quantity units
zero =
    coerce 0.0

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

hypot2 :: Quantity units -> Quantity units -> Quantity units
hypot2 (Quantity a) (Quantity b) =
    Quantity (Prelude.sqrt (a Prelude.* a Prelude.+ b Prelude.* b))

hypot3 :: Quantity units -> Quantity units -> Quantity units -> Quantity units
hypot3 (Quantity a) (Quantity b) (Quantity c) =
    Quantity (Prelude.sqrt (a Prelude.* a Prelude.+ b Prelude.* b Prelude.+ c Prelude.* c))
