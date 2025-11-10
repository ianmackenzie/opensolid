module OpenSolid.Volume
  ( Volume
  , zero
  , cubicMeters
  , inCubicMeters
  , cubicMeter
  , cubicCentimeter
  , cubicCentimeters
  , inCubicCentimeters
  )
where

import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units (CubicMeters)

type Volume = Quantity CubicMeters

zero :: Volume
zero = Quantity.zero

cubicMeter :: Volume
cubicMeter = cubicMeters 1

cubicMeters :: Number -> Volume
cubicMeters = Quantity.coerce

inCubicMeters :: Volume -> Number
inCubicMeters = Quantity.coerce

cubicCentimeter :: Volume
cubicCentimeter = Length.centimeter .*. Length.centimeter .*. Length.centimeter

cubicCentimeters :: Number -> Volume
cubicCentimeters = (.*. cubicCentimeter)

inCubicCentimeters :: Volume -> Number
inCubicCentimeters = (./. cubicCentimeter)
