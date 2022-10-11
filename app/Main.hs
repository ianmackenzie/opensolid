module Main (main) where

import qualified Area
import qualified Debug
import qualified Direction2d
import Expression1d (Expression1d)
import qualified Expression1d
import Expression1d.Root (Root)
import qualified Expression1d.Root as Root
import qualified Interval
import qualified Length
import OpenSolid
import Point2d (Point2d)
import qualified Point2d
import qualified Quantity
import qualified String
import qualified Vector2d
import qualified Vector3d
import qualified Volume

data MyPoints = MyPoints !(Point2d ()) !(Point2d ()) deriving (Show)

showRoot :: Expression1d Unitless -> Root -> String
showRoot x root =
    String.fromInt (Root.order root) ++ ":" ++ String.fromFloat (Expression1d.evaluate x (Root.value root))

main :: IO ()
main = do
    Debug.log "Integer product" (3 * 4)
    Debug.log "Volume in cubic centimeters" volumeInCubicCentimeters
    Debug.log "Integer division" (10 / 4)
    Debug.log "True division" (10.0 / 4.0)
    Debug.log "Dot product" dotProduct
    Debug.log "Determinant" determinant
    Debug.log "Square root" squareRoot
    Debug.log "Translated point" translatedPoint
    Debug.log "Vector sum" vectorSum
    Debug.log "Cross product" crossProduct
    Debug.log "Scaled vector" scaledVector
    Debug.log "Interval difference" intervalDifference
    Debug.log "Interval product" intervalProduct
    Debug.log "Direction" Direction2d.x
    Debug.log "Tuple" (Point2d.meters 1.0 2.0, Point2d.meters 3.0 4.0)
    Debug.log "Custom type" (MyPoints (Point2d.meters 1.0 2.0) (Point2d.meters 3.0 4.0))
    Debug.log "Roots" [showRoot x root | root <- roots]
    Debug.log "sqrt 2.0" (sqrt 2.0)
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
    crossProduct = Vector3d.meters 1.0 2.0 3.0 >< Vector3d.meters 4.0 5.0 6.0
    scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
    intervalDifference = Interval.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
    intervalProduct = Length.centimeters 20.0 * Interval.from (Length.meters 2.0) (Length.meters 3.0)
    t = Expression1d.parameter
    -- x = -2.0 + t * 4.0
    -- y = Expression1d.squared x - 2.0
    x = 3.0 * t
    y = Expression1d.squared (x - 1.0) * (x - 2.0)
    roots = Expression1d.roots 1e-12 y
