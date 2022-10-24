module Length (
    zero,
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
