module Main (main) where

import qualified Area
import qualified Debug
import qualified Length
import OpenSolid
import qualified Point2d
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
    Debug.log "True division" (10.0 / 4.0)
    Debug.log "Dot product" dotProduct
    Debug.log "Determinant" determinant
    Debug.log "Square root" squareRoot
    Debug.log "Translated point" translatedPoint
    Debug.log "Vector sum" vectorSum
  where
    k = 0.5
    area = Area.squareMeters 3.0
    length = Length.centimeters 3.0
    volume = area * length
    volumeInCubicCentimeters = Volume.inCubicCentimeters volume
    v1 = Vector2d.meters 1.0 2.0
    v2 = k * Vector2d.meters 3.0 4.0
    dotProduct = v1 . v2
    determinant = Vector2d.determinant v1 v2
    squareRoot = sqrt dotProduct
    translatedPoint = Point2d.meters 2.0 3.0 + Vector2d.meters 4.0 5.0
    vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
