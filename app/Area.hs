module Area (
    squareMeters,
    inSquareMeters,
    squareMeter,
) where

import OpenSolid
import Units qualified

squareMeter :: Area
squareMeter = squareMeters 1.0

squareMeters :: Float -> Area
squareMeters = Units.add

inSquareMeters :: Area -> Float
inSquareMeters = Units.drop
