module Area (
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid

squareMeter :: Area
squareMeter = squareMeters 1.0

squareMeters :: Float -> Area
squareMeters (Qty x) = Qty x

inSquareMeters :: Area -> Float
inSquareMeters (Qty x) = Qty x
