module OpenSolid.Area
  ( Area
  , zero
  , squareMeters
  , inSquareMeters
  , squareMeter
  , squareInches
  , inSquareInches
  , squareInch
  )
where

import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty

{-| An area in square meters, square inches etc.

Represented internally as a value in square meters.
-}
type Area = Qty SquareMeters

-- | The zero value.
zero :: Area
zero = Qty.zero

-- | One square meter.
squareMeter :: Area
squareMeter = squareMeters 1.0

-- | One square inch.
squareInch :: Area
squareInch = Length.inch * Length.inch

-- | Construct an area from a number of square meters.
squareMeters :: Float -> Area
squareMeters = Qty.coerce

-- | Convert an area to a number of square meters.
inSquareMeters :: Area -> Float
inSquareMeters = Qty.coerce

-- | Construct an area from a number of square inches.
squareInches :: Float -> Area
squareInches = (* squareInch)

-- | Convert an area to a number of square inches.
inSquareInches :: Area -> Float
inSquareInches = (/ squareInch)
