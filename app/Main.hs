module Main (main) where

import qualified Area
import qualified Debug
import qualified Direction2d
import Expression1d (Expression1d)
import qualified Expression1d
import Expression1d.Root (Root)
import qualified Expression1d.Root as Root
import qualified Length
import qualified List
import OpenSolid
import Point2d (Point2d)
import qualified Point2d
import qualified Qty
import qualified Range
import Script (IOError, Script)
import qualified Script
import qualified String
import qualified Vector2d
import qualified Vector3d
import qualified Volume

data MyPoints = MyPoints !(Point2d ()) !(Point2d ()) deriving (Show)

showRoot :: Expression1d Unitless -> Root -> String
showRoot x root =
    String.fromInt (Root.order root) ++ ":" ++ String.fromFloat (Expression1d.evaluate x (Root.value root))

listTest :: List (Int, Int)
listTest = List.do
    a <- [1 .. 10]
    b <- [1 .. 10]
    [(a, b) | a + b == 10]

script :: Script IOError ()
script = Script.do
    log "Integer product" (3 * 4)
    log "Volume in cubic centimeters" volumeInCubicCentimeters
    log "Integer division" (10 // 4)
    log "True division" (10.0 / 4.0)
    log "Dot product" dotProduct
    log "Determinant" determinant
    log "Square root" squareRoot
    log "Translated point" translatedPoint
    log "Vector sum" vectorSum
    log "Cross product" crossProduct
    log "Scaled vector" scaledVector
    log "Range difference" rangeDifference
    log "Range product" rangeProduct
    log "Direction" Direction2d.x
    log "Tuple" (Point2d.meters 1.0 2.0, Point2d.meters 3.0 4.0)
    log "Custom type" (MyPoints (Point2d.meters 1.0 2.0) (Point2d.meters 3.0 4.0))
    log "Roots" [showRoot x root | root <- roots]
    log "sqrt 2.0" (Qty.sqrt 2.0)
    log "List test" listTest
  where
    log label value = Script.printLine (label ++ ": " ++ Debug.toString value)
    k = 0.5
    area = Area.squareMeters 3.0
    length = Length.centimeters 3.0
    volume = area * length
    volumeInCubicCentimeters = Volume.inCubicCentimeters volume
    v1 = Vector2d.meters 1.0 2.0
    v2 = k * Vector2d.meters 3.0 4.0
    dotProduct = v1 <> v2
    determinant = Vector2d.determinant v1 v2
    squareRoot = Area.sqrt dotProduct
    translatedPoint = Point2d.meters 2.0 3.0 |> Point2d.translateBy (Vector2d.meters 4.0 5.0)
    vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
    crossProduct = Vector3d.meters 1.0 2.0 3.0 >< Vector3d.meters 4.0 5.0 6.0
    scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
    rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
    rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
    t = Expression1d.parameter
    x = 3.0 * t
    y = Expression1d.squared (x - 1.0) * (x - 2.0)
    roots = Expression1d.roots 1e-12 y

main :: Script.Program
main =
    Script.run script
