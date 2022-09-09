module Length (
    Length,
    Meters,
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
import qualified Show
import Units (Meters)

type Length = Quantity Meters

instance Show Length where
    showsPrec precedence length =
        Show.primitive precedence "Length.meters" [inMeters length]

meter :: Length
meter =
    Quantity.baseUnit

meters :: Float -> Length
meters =
    Quantity.baseUnits

inMeters :: Length -> Float
inMeters =
    Quantity.inBaseUnits

centimeter :: Length
centimeter =
    meters 0.01

centimeters :: Float -> Length
centimeters =
    (* centimeter)

inCentimeters :: Length -> Float
inCentimeters =
    (/ centimeter)
