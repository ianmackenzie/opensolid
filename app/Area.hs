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

type Area = Quantity SquareMeters

instance Show Area where
    showsPrec precedence area =
        Show.primitive precedence "Area.squareMeters" [inSquareMeters area]

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
