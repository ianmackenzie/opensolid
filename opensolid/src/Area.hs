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

type Area = Qty SquareMeters

zero :: Area
zero = Qty.zero

squareMeter :: Area
squareMeter = squareMeters 1.0

squareMeters :: Float -> Area
squareMeters = Qty

inSquareMeters :: Area -> Float
inSquareMeters (Qty x) = x
