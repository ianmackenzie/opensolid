module Volume (
    Volume,
    CubicMeters,
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
import Quantity (Quantity)
import qualified Quantity
import qualified Show
import Units (CubicMeters)

type Volume = Quantity CubicMeters

instance Show Volume where
    showsPrec precedence volume =
        Show.primitive precedence "Volume.cubicMeters" [inCubicMeters volume]

zero :: Volume
zero =
    Quantity.zero

cubicMeter :: Volume
cubicMeter = Quantity.baseUnit

cubicMeters :: Float -> Volume
cubicMeters = Quantity.baseUnits

inCubicMeters :: Volume -> Float
inCubicMeters = Quantity.inBaseUnits

cubicCentimeter :: Volume
cubicCentimeter = Length.centimeter * Length.centimeter * Length.centimeter

cubicCentimeters :: Float -> Volume
cubicCentimeters = (* cubicCentimeter)

inCubicCentimeters :: Volume -> Float
inCubicCentimeters = (/ cubicCentimeter)
