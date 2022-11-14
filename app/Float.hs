module Float (
    zero,
    positiveInfinity,
    negativeInfinity,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    sqrt,
    abs,
    clamp,
) where

import OpenSolid
import qualified Qty

zero :: Float
zero =
    Qty.zero

positiveInfinity :: Float
positiveInfinity =
    Qty.positiveInfinity

negativeInfinity :: Float
negativeInfinity =
    Qty.negativeInfinity

infinity :: Float
infinity =
    Qty.infinity

isNaN :: Float -> Bool
isNaN =
    Qty.isNaN

interpolateFrom :: Float -> Float -> Float -> Float
interpolateFrom =
    Qty.interpolateFrom

midpoint :: Float -> Float -> Float
midpoint =
    Qty.midpoint

abs :: Float -> Float
abs =
    Qty.abs

clamp :: Float -> Float -> Float -> Float
clamp =
    Qty.clamp

sqrt :: Float -> Float
sqrt =
    Qty.sqrt
