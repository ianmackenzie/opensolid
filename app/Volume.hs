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
import qualified Qty
import qualified Units

zero :: Volume
zero =
    Qty.zero

positiveInfinity :: Volume
positiveInfinity =
    Qty.positiveInfinity

negativeInfinity :: Volume
negativeInfinity =
    Qty.negativeInfinity

infinity :: Volume
infinity =
    Qty.infinity

isNaN :: Volume -> Bool
isNaN =
    Qty.isNaN

interpolateFrom :: Volume -> Volume -> Float -> Volume
interpolateFrom =
    Qty.interpolateFrom

midpoint :: Volume -> Volume -> Volume
midpoint =
    Qty.midpoint

abs :: Volume -> Volume
abs =
    Qty.abs

clamp :: Volume -> Volume -> Volume -> Volume
clamp =
    Qty.clamp

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
