module Volume (
    zero,
    positiveInfinity,
    negativeInfinity,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    abs,
    clamp,
    cubicMeters,
    inCubicMeters,
    cubicMeter,
    cubicCentimeter,
    cubicCentimeters,
    inCubicCentimeters,
) where

import qualified Length
import OpenSolid
import qualified Scalar
import qualified Units

zero :: Volume
zero =
    Scalar.zero

positiveInfinity :: Volume
positiveInfinity =
    Scalar.positiveInfinity

negativeInfinity :: Volume
negativeInfinity =
    Scalar.negativeInfinity

infinity :: Volume
infinity =
    Scalar.infinity

isNaN :: Volume -> Bool
isNaN =
    Scalar.isNaN

interpolateFrom :: Volume -> Volume -> Float -> Volume
interpolateFrom =
    Scalar.interpolateFrom

midpoint :: Volume -> Volume -> Volume
midpoint =
    Scalar.midpoint

abs :: Volume -> Volume
abs =
    Scalar.abs

clamp :: Volume -> Volume -> Volume -> Volume
clamp =
    Scalar.clamp

cubicMeter :: Volume
cubicMeter =
    cubicMeters 1.0

cubicMeters :: Float -> Volume
cubicMeters =
    Units.add

inCubicMeters :: Volume -> Float
inCubicMeters =
    Units.drop

cubicCentimeter :: Volume
cubicCentimeter =
    Length.centimeter * Length.centimeter * Length.centimeter

cubicCentimeters :: Float -> Volume
cubicCentimeters =
    (* cubicCentimeter)

inCubicCentimeters :: Volume -> Float
inCubicCentimeters =
    (/ cubicCentimeter)
