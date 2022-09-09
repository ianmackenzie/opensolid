module Area (
    Area,
    SquareMeters,
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified Show
import Units (SquareMeters)

type Area = Quantity SquareMeters

instance Show Area where
    showsPrec precedence area =
        Show.primitive precedence "Area.squareMeters" [inSquareMeters area]

squareMeter :: Area
squareMeter =
    Quantity.baseUnit

squareMeters :: Float -> Area
squareMeters =
    Quantity.baseUnits

inSquareMeters :: Area -> Float
inSquareMeters =
    Quantity.inBaseUnits
