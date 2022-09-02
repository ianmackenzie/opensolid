module Area (
    Area,
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import Quantity (Quantity (..))
import qualified Quantity
import Units (SquareMeters)

type Area = Quantity SquareMeters

squareMeter :: Area
squareMeter = Quantity.baseUnit

squareMeters :: Float -> Area
squareMeters = Quantity.baseUnits

inSquareMeters :: Area -> Float
inSquareMeters = Quantity.inBaseUnits
