module Area
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

import Length qualified
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import Units (SquareMeters)
import Units qualified

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
squareMeters = Units.coerce

-- | Convert an area to a number of square meters.
inSquareMeters :: Area -> Float
inSquareMeters = Units.coerce

-- | Construct an area from a number of square inches.
squareInches :: Float -> Area
squareInches = (* squareInch)

-- | Convert an area to a number of square inches.
inSquareInches :: Area -> Float
inSquareInches = (/ squareInch)
