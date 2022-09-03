module Area (
    Area,
    SquareMeters,
    zero,
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import Quantity (Quantity (..))
import qualified Quantity
import qualified String
import Units (SquareMeters)

type Area = Quantity SquareMeters

instance Show Area where
    show area = show ("Area.squareMeters " ++ String.fromFloat (inSquareMeters area))

zero :: Area
zero = Quantity.zero

squareMeter :: Area
squareMeter = Quantity.baseUnit

squareMeters :: Float -> Area
squareMeters = Quantity.baseUnits

inSquareMeters :: Area -> Float
inSquareMeters = Quantity.inBaseUnits
