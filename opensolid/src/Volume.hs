module Volume
  ( Volume
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
import Units (CubicMeters)

type Volume = Qty CubicMeters

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