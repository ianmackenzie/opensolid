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
import qualified Scalar

zero :: Float
zero =
    Scalar.zero

positiveInfinity :: Float
positiveInfinity =
    Scalar.positiveInfinity

negativeInfinity :: Float
negativeInfinity =
    Scalar.negativeInfinity

infinity :: Float
infinity =
    Scalar.infinity

isNaN :: Float -> Bool
isNaN =
    Scalar.isNaN

interpolateFrom :: Float -> Float -> Float -> Float
interpolateFrom =
    Scalar.interpolateFrom

midpoint :: Float -> Float -> Float
midpoint =
    Scalar.midpoint

abs :: Float -> Float
abs =
    Scalar.abs

clamp :: Float -> Float -> Float -> Float
clamp =
    Scalar.clamp

sqrt :: Float -> Float
sqrt =
    Scalar.sqrt
