module Length
  ( Length
  , meter
  , centimeter
  , meters
  , inMeters
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

centimeter :: Length
centimeter = meters 0.01

centimeters :: Float -> Length
centimeters = (* centimeter)

inCentimeters :: Length -> Float
inCentimeters = (/ centimeter)
