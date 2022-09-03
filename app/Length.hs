module Length (
    Length,
    Meters,
    zero,
    meter,
    centimeter,
    meters,
    inMeters,
    centimeters,
    inCentimeters,
) where

import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import Units (Meters)

type Length = Quantity Meters

zero :: Length
zero = Quantity.zero

meter :: Length
meter = Quantity.baseUnit

meters :: Float -> Length
meters = Quantity.baseUnits

inMeters :: Length -> Float
inMeters = Quantity.inBaseUnits

centimeter :: Length
centimeter = meters 0.01

centimeters :: Float -> Length
centimeters = (* centimeter)

inCentimeters :: Length -> Float
inCentimeters = (/ centimeter)
