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
import qualified Scalar
import UnitCoercion

zero :: Length
zero =
    Scalar.zero

positiveInfinity :: Length
positiveInfinity =
    Scalar.positiveInfinity

negativeInfinity :: Length
negativeInfinity =
    Scalar.negativeInfinity

infinity :: Length
infinity =
    Scalar.infinity

isNaN :: Length -> Bool
isNaN =
    Scalar.isNaN

interpolateFrom :: Length -> Length -> Float -> Length
interpolateFrom =
    Scalar.interpolateFrom

midpoint :: Length -> Length -> Length
midpoint =
    Scalar.midpoint

abs :: Length -> Length
abs =
    Scalar.abs

clamp :: Length -> Length -> Length -> Length
clamp =
    Scalar.clamp

meter :: Length
meter =
    meters 1.0

meters :: Float -> Length
meters =
    addUnits

inMeters :: Length -> Float
inMeters =
    dropUnits

centimeter :: Length
centimeter =
    meters 0.01

centimeters :: Float -> Length
centimeters =
    (* centimeter)

inCentimeters :: Length -> Float
inCentimeters =
    (/ centimeter)
