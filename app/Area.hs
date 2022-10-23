module Area (
    Area,
    SquareMeters,
    zero,
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import qualified Quantity
import qualified Show
import Units (SquareMeters)

zero :: Area
zero =
    Quantity.zero

squareMeter :: Area
squareMeter =
    Quantity.baseUnit

squareMeters :: Float -> Area
squareMeters =
    Quantity.baseUnits

inSquareMeters :: Area -> Float
inSquareMeters =
    Quantity.inBaseUnits
