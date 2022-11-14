module Length (
    zero,
    positiveInfinity,
    negativeInfinity,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    abs,
    clamp,
    meter,
    centimeter,
    meters,
    inMeters,
    centimeters,
    inCentimeters,
) where

import OpenSolid
import qualified Qty
import qualified Units

zero :: Length
zero =
    Qty.zero

positiveInfinity :: Length
positiveInfinity =
    Qty.positiveInfinity

negativeInfinity :: Length
negativeInfinity =
    Qty.negativeInfinity

infinity :: Length
infinity =
    Qty.infinity

isNaN :: Length -> Bool
isNaN =
    Qty.isNaN

interpolateFrom :: Length -> Length -> Float -> Length
interpolateFrom =
    Qty.interpolateFrom

midpoint :: Length -> Length -> Length
midpoint =
    Qty.midpoint

abs :: Length -> Length
abs =
    Qty.abs

clamp :: Length -> Length -> Length -> Length
clamp =
    Qty.clamp

meter :: Length
meter =
    meters 1.0

meters :: Float -> Length
meters =
    Units.add

inMeters :: Length -> Float
inMeters =
    Units.drop

centimeter :: Length
centimeter =
    meters 0.01

centimeters :: Float -> Length
centimeters =
    (* centimeter)

inCentimeters :: Length -> Float
inCentimeters =
    (/ centimeter)
