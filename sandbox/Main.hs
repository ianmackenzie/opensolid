module Main (main) where

import Angle qualified
import Area qualified
import Curve1d qualified
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Direction3d ()
import Float qualified
import Length qualified
import List qualified
import Maybe qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import QuadraticSpline2d (QuadraticSpline2d (QuadraticSpline2d))
import Range qualified
import Script (IOError, Script)
import Script qualified
import Vector2d qualified
import Vector3d qualified
import Volume qualified

data MyPoints = MyPoints (Point2d ()) (Point2d ()) deriving (Show)

listTest :: List (Int, Int)
listTest = do
    a <- [1 .. 10]
    b <- [1 .. 10]
    [(a, b) | a + b == 10]

equalWithin :: "tolerance" ::: Length -> Length -> Length -> Bool
equalWithin tolerance x y = Qty.abs (x - y) <= tolerance

testSpline :: QuadraticSpline2d coordinates
testSpline =
    QuadraticSpline2d
        (Point2d.meters 0.0 0.0)
        (Point2d.meters 1.0 2.0)
        (Point2d.meters 2.0 0.0)

script :: Script IOError ()
script = do
    log "Integer product" (3 * 4)
    log "Volume in cubic centimeters" volumeInCubicCentimeters
    log "Integer division" (10 // 4)
    log "True division" (10 / 4)
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
    log "Roots" roots
    log "sqrt 2.0" (Qty.sqrt 2.0)
    log "List test" listTest
    log "Equality test" (equalWithin Length.centimeter (Length.meters 1.0) (Length.meters 1.005))
    log "Roots" expressionRoots
    log "Or test" (Vector3d.direction Vector3d.zero |> Maybe.orErr "Zero vector")
    log "Collapse test" (List.collapse joinStringChunks stringChunks |> List.filter (/= " "))
    log "Start parameter value" (Curve2d.parameterValue tolerance Point2d.origin testSpline)
    log "End parameter value" (Curve2d.parameterValue tolerance (Point2d.meters 2.0 0.0) testSpline)
    log "Mid parameter value" (Curve2d.parameterValue tolerance (Point2d.meters 1.0 1.0) testSpline)
    log "Off-curve parameter value" (Curve2d.parameterValue tolerance (Point2d.meters 1.0 1.1) testSpline)
  where
    tolerance = Length.meters 1e-9
    log label value = Script.printLine (label ++ ": " ++ Debug.show value)
    k = 0.5
    area = Area.squareMeters 3.0
    length = Length.centimeters 3.0
    volume = area * length
    volumeInCubicCentimeters = Volume.inCubicCentimeters volume
    v1 = Vector2d.meters 1.0 2.0
    v2 = k * Vector2d.meters 3.0 4.0
    dotProduct = v1 <> v2
    determinant = Vector2d.determinant v1 v2
    squareRoot = Qty.sqrt dotProduct
    translatedPoint = Point2d.meters 2.0 3.0 |> Point2d.translateBy (Vector2d.meters 4.0 5.0)
    vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
    crossProduct = Vector3d.meters 1.0 2.0 3.0 >< Vector3d.meters 4.0 5.0 6.0
    scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
    rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
    rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
    t = Curve1d.parameter
    x = 3.0 * t
    y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
    roots = Curve1d.roots 1e-12 y
    theta = Angle.radians (2 * Float.pi) * t
    expression = Curve1d.squared (Curve1d.sin theta)
    expressionRoots = Curve1d.roots 1e-12 expression
    joinStringChunks " " _ = Nothing
    joinStringChunks _ " " = Nothing
    joinStringChunks s1 s2 = Just (s1 ++ s2)
    stringChunks = ["T", "h", "is", " ", "i", "s", " ", "a", " ", "t", "es", "t"]

main :: IO ()
main = Script.run script
