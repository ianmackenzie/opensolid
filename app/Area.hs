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
import qualified Scalar
import UnitCoercion

zero :: Area
zero =
    Scalar.zero

positiveInfinity :: Area
positiveInfinity =
    Scalar.positiveInfinity

negativeInfinity :: Area
negativeInfinity =
    Scalar.negativeInfinity

infinity :: Area
infinity =
    Scalar.infinity

isNaN :: Area -> Bool
isNaN =
    Scalar.isNaN

interpolateFrom :: Area -> Area -> Float -> Area
interpolateFrom =
    Scalar.interpolateFrom

midpoint :: Area -> Area -> Area
midpoint =
    Scalar.midpoint

abs :: Area -> Area
abs =
    Scalar.abs

clamp :: Area -> Area -> Area -> Area
clamp =
    Scalar.clamp

sqrt :: Area -> Length
sqrt =
    Scalar.sqrt

squareMeter :: Area
squareMeter =
    squareMeters 1.0

squareMeters :: Float -> Area
squareMeters =
    addUnits

inSquareMeters :: Area -> Float
inSquareMeters =
    dropUnits
