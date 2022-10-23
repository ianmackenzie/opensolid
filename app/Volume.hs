module Volume (
    Volume,
    zero,
    cubicMeters,
    inCubicMeters,
    cubicMeter,
    cubicCentimeter,
    cubicCentimeters,
    inCubicCentimeters,
) where

import qualified Length
import OpenSolid
import qualified Quantity

zero :: Volume
zero =
    Quantity.zero

cubicMeter :: Volume
cubicMeter =
    Quantity.baseUnit

cubicMeters :: Float -> Volume
cubicMeters =
    Quantity.baseUnits

inCubicMeters :: Volume -> Float
inCubicMeters =
    Quantity.inBaseUnits

cubicCentimeter :: Volume
cubicCentimeter =
    Length.centimeter * Length.centimeter * Length.centimeter

cubicCentimeters :: Float -> Volume
cubicCentimeters =
    (* cubicCentimeter)

inCubicCentimeters :: Volume -> Float
inCubicCentimeters =
    (/ cubicCentimeter)
