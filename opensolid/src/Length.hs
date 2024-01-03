module Length
  ( Length
  , meter
  , millimeter
  , centimeter
  , meters
  , inMeters
  , millimeters
  , inMillimeters
  , centimeters
  , inCentimeters
  )
where

import OpenSolid
import Units (Meters)

type Length = Qty Meters

meter :: Length
meter = meters 1.0

meters :: Float -> Length
meters (Qty x) = Qty x

inMeters :: Length -> Float
inMeters (Qty x) = Qty x

millimeter :: Length
millimeter = meters 0.001

millimeters :: Float -> Length
millimeters = (* millimeter)

inMillimeters :: Length -> Float
inMillimeters = (/ millimeter)

centimeter :: Length
centimeter = meters 0.01

centimeters :: Float -> Length
centimeters = (* centimeter)

inCentimeters :: Length -> Float
inCentimeters = (/ centimeter)
