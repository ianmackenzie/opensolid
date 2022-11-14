module Area (
    zero,
    positiveInfinity,
    negativeInfinity,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    abs,
    clamp,
    sqrt,
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import qualified Qty
import qualified Units

zero :: Area
zero =
    Qty.zero

positiveInfinity :: Area
positiveInfinity =
    Qty.positiveInfinity

negativeInfinity :: Area
negativeInfinity =
    Qty.negativeInfinity

infinity :: Area
infinity =
    Qty.infinity

isNaN :: Area -> Bool
isNaN =
    Qty.isNaN

interpolateFrom :: Area -> Area -> Float -> Area
interpolateFrom =
    Qty.interpolateFrom

midpoint :: Area -> Area -> Area
midpoint =
    Qty.midpoint

abs :: Area -> Area
abs =
    Qty.abs

clamp :: Area -> Area -> Area -> Area
clamp =
    Qty.clamp

sqrt :: Area -> Length
sqrt =
    Qty.sqrt

squareMeter :: Area
squareMeter =
    squareMeters 1.0

squareMeters :: Float -> Area
squareMeters =
    Units.add

inSquareMeters :: Area -> Float
inSquareMeters =
    Units.drop
