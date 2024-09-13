module Area
  ( Area
  , zero
  , squareMeters
  , inSquareMeters
  , squareMeter
  )
where

import OpenSolid
import Qty qualified
import Units (SquareMeters)
import Units qualified

type Area = Qty SquareMeters

zero :: Area
zero = Qty.zero

squareMeter :: Area
squareMeter = squareMeters 1.0

squareMeters :: Float -> Area
squareMeters = Units.coerce

inSquareMeters :: Area -> Float
inSquareMeters = Units.coerce
