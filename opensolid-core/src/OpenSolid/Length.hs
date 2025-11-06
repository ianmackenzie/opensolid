module OpenSolid.Length
  ( Length
  , zero
  , meter
  , millimeter
  , centimeter
  , micrometer
  , nanometer
  , meters
  , inMeters
  , mm
  , millimeters
  , inMillimeters
  , cm
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

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

{-| A length in millimeters, meters, inches etc.

Represented internally as a value in meters.
-}
type Length = Quantity Meters

-- | The zero value.
zero :: Length
zero = Quantity.zero

-- | One meter.
meter :: Length
meter = meters 1.0

-- | Construct a length from a number of meters.
meters :: Number -> Length
meters = Quantity.coerce

-- | Convert a length to a number of meters.
inMeters :: Length -> Number
inMeters = Quantity.coerce

-- | One millimeter.
millimeter :: Length
millimeter = meters 0.001

-- | Construct a length value from a number of millimeters.
millimeters :: Number -> Length
millimeters = (.*. millimeter)

{-| Construct a length value from a number of millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Length
mm = millimeters

-- | Convert a length to a number of millimeters.
inMillimeters :: Length -> Number
inMillimeters = (./. millimeter)

-- | One centimeter.
centimeter :: Length
centimeter = meters 0.01

-- | Construct a length from a number of centimeters.
centimeters :: Number -> Length
centimeters = (.*. centimeter)

{-| Construct a length from a number of centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Length
cm = centimeters

-- | Convert a length to a number of centimeters.
inCentimeters :: Length -> Number
inCentimeters = (./. centimeter)

-- | One micrometer.
micrometer :: Length
micrometer = meters 1e-6

-- | Construct a length from a number of micrometers.
micrometers :: Number -> Length
micrometers = (.*. micrometer)

-- | Convert a length to a number of micrometers.
inMicrometers :: Length -> Number
inMicrometers = (./. micrometer)

-- | One nanometer.
nanometer :: Length
nanometer = meters 1e-9

-- | Construct a length from a number of nanometers.
nanometers :: Number -> Length
nanometers = (.*. nanometer)

-- | Convert a length to a number of nanometers.
inNanometers :: Length -> Number
inNanometers = (./. nanometer)

-- | One inch.
inch :: Length
inch = millimeters 25.4

-- | Construct a length from a number of inches.
inches :: Number -> Length
inches = (.*. inch)

-- | Convert a length to a number of inches.
inInches :: Length -> Number
inInches = (./. inch)

-- | One CSS pixel, equal to 1/96 of an inch.
pixel :: Length
pixel = inch ./ 96.0

-- | Construct a length from a number of CSS pixels.
pixels :: Number -> Length
pixels = (.*. pixel)

-- | Convert a length into a number of CSS pixels.
inPixels :: Length -> Number
inPixels = (./. pixel)
