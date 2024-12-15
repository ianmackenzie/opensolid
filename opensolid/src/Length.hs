module Length
  ( Length
  , zero
  , meter
  , millimeter
  , centimeter
  , micrometer
  , nanometer
  , meters
  , inMeters
  , millimeters
  , inMillimeters
  , centimeters
  , inCentimeters
  , micrometers
  , inMicrometers
  , nanometers
  , inNanometers
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

-- | The zero value.
zero :: Length
zero = Qty.zero

-- | One meter.
meter :: Length
meter = meters 1.0

-- | Construct a length from a number of meters.
meters :: Float -> Length
meters = Units.coerce

-- | Convert a length to a number of meters.
inMeters :: Length -> Float
inMeters = Units.coerce

-- | One millimeter.
millimeter :: Length
millimeter = meters 0.001

-- | Construct a length value from a number of millimeters.
millimeters :: Float -> Length
millimeters = (* millimeter)

-- | Convert a length to a number of millimeters.
inMillimeters :: Length -> Float
inMillimeters = (/ millimeter)

-- | One centimeter.
centimeter :: Length
centimeter = meters 0.01

-- | Construct a length from a number of centimeters.
centimeters :: Float -> Length
centimeters = (* centimeter)

-- | Convert a length to a number of centimeters.
inCentimeters :: Length -> Float
inCentimeters = (/ centimeter)

-- | One micrometer.
micrometer :: Length
micrometer = meters 1e-6

-- | Construct a length from a number of micrometers.
micrometers :: Float -> Length
micrometers = (* micrometer)

-- | Convert a length to a number of micrometers.
inMicrometers :: Length -> Float
inMicrometers = (/ micrometer)

-- | One nanometer.
nanometer :: Length
nanometer = meters 1e-9

-- | Construct a length from a number of nanometers.
nanometers :: Float -> Length
nanometers = (* nanometer)

-- | Convert a length to a number of nanometers.
inNanometers :: Length -> Float
inNanometers = (/ nanometer)

-- | One inch.
inch :: Length
inch = millimeters 25.4

-- | Construct a length from a number of inches.
inches :: Float -> Length
inches = (* inch)

-- | Convert a length to a number of inches.
inInches :: Length -> Float
inInches = (/ inch)

-- | One CSS pixel, equal to 1/96 of an inch.
pixel :: Length
pixel = inch / 96.0

-- | Construct a length from a number of CSS pixels.
pixels :: Float -> Length
pixels = (* pixel)

-- | Convert a length into a number of CSS pixels.
inPixels :: Length -> Float
inPixels = (/ pixel)
