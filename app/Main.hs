module Main (main) where

import qualified Area
import qualified Debug
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import Units (Meters)
import Vector2d (Vector2d)
import qualified Vector2d
import qualified Volume

main :: IO ()
main = do
    Debug.log "Integer product" (3 * 4)
    Debug.log "Volume in cubic centimeters" volumeInCubicCentimeters
    Debug.log "Integer division" (10 // 4)
    Debug.log "Dot product" dotProduct
    Debug.log "Cross product" crossProduct
    Debug.log "Square root" squareRoot
  where
    k = 0.5
    area = Area.squareMeters 3.0
    length = Length.centimeters 3.0
    volume = area * length
    volumeInCubicCentimeters = Volume.inCubicCentimeters volume
    v1 = Vector2d.meters 1.0 2.0
    v2 = k * Vector2d.meters 3.0 4.0
    dotProduct = Vector2d.dotProduct v1 v2
    crossProduct = Vector2d.crossProduct v1 v2
    squareRoot = sqrt dotProduct
