module Volume
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

import Length qualified
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import Units (CubicMeters)
import Units qualified

type Volume = Qty CubicMeters

zero :: Volume
zero = Qty.zero

cubicMeter :: Volume
cubicMeter = cubicMeters 1.0

cubicMeters :: Float -> Volume
cubicMeters = Units.coerce

inCubicMeters :: Volume -> Float
inCubicMeters = Units.coerce

cubicCentimeter :: Volume
cubicCentimeter = Length.centimeter * Length.centimeter * Length.centimeter

cubicCentimeters :: Float -> Volume
cubicCentimeters = (* cubicCentimeter)

inCubicCentimeters :: Volume -> Float
inCubicCentimeters = (/ cubicCentimeter)
