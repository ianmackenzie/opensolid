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
import Units qualified

{-| A length in millimeters, meters, inches etc.

Represented internally as a value in meters.
-}
type Length = Qty Meters

zero :: Length
zero = Qty.zero

meter :: Length
meter = meters 1.0

meters :: Float -> Length
meters = Units.coerce

inMeters :: Length -> Float
inMeters = Units.coerce

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
