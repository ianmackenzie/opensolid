module OpenSolid.Area
  ( Area
  , sqrt
  , zero
  , squareMeters
  , inSquareMeters
  , squareMeter
  , squareInches
  , inSquareInches
  , squareInch
  )
where

import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

{-| An area in square meters, square inches etc.

Represented internally as a value in square meters.
-}
type Area = Quantity SquareMeters

sqrt :: Area -> Length
sqrt = Quantity.sqrt

-- | The zero value.
zero :: Area
zero = Quantity.zero

-- | One square meter.
squareMeter :: Area
squareMeter = squareMeters 1.0

-- | One square inch.
squareInch :: Area
squareInch = Length.inch * Length.inch

-- | Construct an area from a number of square meters.
squareMeters :: Number -> Area
squareMeters = Quantity.coerce

-- | Convert an area to a number of square meters.
inSquareMeters :: Area -> Number
inSquareMeters = Quantity.coerce

-- | Construct an area from a number of square inches.
squareInches :: Number -> Area
squareInches = (* squareInch)

-- | Convert an area to a number of square inches.
inSquareInches :: Area -> Number
inSquareInches = (./. squareInch)
