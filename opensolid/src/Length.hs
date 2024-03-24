module Length
  ( Length
  , zero
  , meter
  , millimeter
  , centimeter
  , meters
  , inMeters
  , millimeters
  , inMillimeters
  , centimeters
  , inCentimeters
  , inch
  , inches
  , inInches
  , pixel
  , pixels
  , inPixels
  )
where

import OpenSolid
import Qty qualified
import Units (Meters)

type Length = Qty Meters

zero :: Length
zero = Qty.zero

meter :: Length
meter = meters 1.0

meters :: Float -> Length
meters = Qty

inMeters :: Length -> Float
inMeters (Qty x) = x

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

inch :: Length
inch = millimeters 25.4

inches :: Float -> Length
inches = (* inch)

inInches :: Length -> Float
inInches = (/ inch)

pixel :: Length
pixel = inch / 96.0

pixels :: Float -> Length
pixels = (* pixel)

inPixels :: Length -> Float
inPixels = (/ pixel)
