module Length (
    meter,
    centimeter,
    meters,
    inMeters,
    centimeters,
    inCentimeters,
) where

import OpenSolid
import qualified Units

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
