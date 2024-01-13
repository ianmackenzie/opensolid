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
import OpenSolid
import Qty qualified
import Units (CubicMeters)

type Volume = Qty CubicMeters

zero :: Volume
zero = Qty.zero

cubicMeter :: Volume
cubicMeter = cubicMeters 1.0

cubicMeters :: Float -> Volume
cubicMeters (Qty x) = Qty x

inCubicMeters :: Volume -> Float
inCubicMeters (Qty x) = Qty x

cubicCentimeter :: Volume
cubicCentimeter = Length.centimeter * Length.centimeter * Length.centimeter

cubicCentimeters :: Float -> Volume
cubicCentimeters = (* cubicCentimeter)

inCubicCentimeters :: Volume -> Float
inCubicCentimeters = (/ cubicCentimeter)
