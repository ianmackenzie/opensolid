module Main (main) where

import Angle qualified
import Area qualified
import Curve1d qualified
import Curve2d (Curve2d (..))
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
import QuadraticSpline2d qualified
import Range qualified
import Script qualified
import Vector2d qualified
import Vector3d qualified
import Volume qualified

log :: Show a => Text -> a -> Script.Program
log label value = Script.printLine (label ++ ": " ++ Debug.show value)

data MyPoints = MyPoints (Point2d ()) (Point2d ()) deriving (Show)

listTest :: List (Int, Int)
listTest = do
  a <- [1 .. 10]
  b <- [1 .. 10]
  [(a, b) | a + b == 10]

joinTextChunks :: Text -> Text -> Maybe Text
joinTextChunks " " _ = Nothing
joinTextChunks _ " " = Nothing
joinTextChunks s1 s2 = Just (s1 ++ s2)

testListCollapse :: Script.Program
testListCollapse =
  let textChunks = ["T", "h", "is", " ", "i", "s", " ", "a", " ", "t", "es", "t"]
   in log "Collapsed list" (List.collapse joinTextChunks textChunks |> List.filter (/= " "))

testCurveFind :: Script.Program
testCurveFind = do
  let p1 = Point2d.meters 0.0 0.0
  let p2 = Point2d.meters 1.0 2.0
  let p3 = Point2d.meters 2.0 0.0
  let testSpline = Curve2d (QuadraticSpline2d.fromControlPoints p1 p2 p3)
  log "Start parameter value" (Curve2d.find Point2d.origin testSpline)
  log "End parameter value" (Curve2d.find (Point2d.meters 2.0 0.0) testSpline)
  log "Mid parameter value" (Curve2d.find (Point2d.meters 1.0 1.0) testSpline)
  log "Off-curve parameter value" (Curve2d.find (Point2d.meters 1.0 1.1) testSpline)
  where
    ?tolerance = Length.meters 1e-9

script :: Script.Program
script = do
  log "Integer product" (3 * 4)
  log "Integer division" (10 // 4)
  log "True division" (10 / 4)
  let area = Area.squareMeters 3.0
  let length = Length.centimeters 3.0
  let volume = area * length
  let volumeInCubicCentimeters = Volume.inCubicCentimeters volume
  log "Volume in cubic centimeters" volumeInCubicCentimeters
  let v1 = Vector2d.meters 1.0 2.0
  let v2 = 0.5 * Vector2d.meters 3.0 4.0
  let dotProduct = v1 <> v2
  log "Dot product" dotProduct
  log "2D cross product" (v1 >< v2)
  let squareRoot = Qty.sqrt dotProduct
  log "Square root" squareRoot
  let translatedPoint = Point2d.meters 2.0 3.0 |> Point2d.translateBy (Vector2d.meters 4.0 5.0)
  log "Translated point" translatedPoint
  let vectorSum = Vector2d.meters 1.0 2.0 + Vector2d.meters 2.0 3.0
  log "Vector sum" vectorSum
  let crossProduct = Vector3d.meters 1.0 2.0 3.0 >< Vector3d.meters 4.0 5.0 6.0
  log "Cross product" crossProduct
  let scaledVector = Length.meters 2.0 * Vector2d.meters 3.0 4.0
  log "Scaled vector" scaledVector
  let rangeDifference = Range.from (Length.meters 2.0) (Length.meters 3.0) - Length.centimeters 50.0
  log "Range difference" rangeDifference
  let rangeProduct = Length.centimeters 20.0 * Range.from (Length.meters 2.0) (Length.meters 3.0)
  log "Range product" rangeProduct
  log "Direction" Direction2d.x
  log "Tuple" (Point2d.meters 1.0 2.0, Point2d.meters 3.0 4.0)
  log "List test" listTest
  log "Custom type" (MyPoints (Point2d.meters 1.0 2.0) (Point2d.meters 3.0 4.0))
  let t = Curve1d.parameter
  let x = 3.0 * t
  let y = (x - 1.0) * (x - 1.0) * (x - 1.0) - (x - 1.0)
  let roots = let ?tolerance = 1e-12 in Curve1d.roots y
  log "Roots" roots
  log "sqrt 2.0" (Qty.sqrt 2.0)
  log "Equality test" (let ?tolerance = Length.centimeter in Length.meters 1.0 ~= Length.meters 1.005)
  let theta = Angle.radians (2 * Float.pi) * t
  let expression = Curve1d.squared (Curve1d.sin theta)
  let expressionRoots = let ?tolerance = 1e-12 in Curve1d.roots expression
  log "Roots" expressionRoots
  testCurveFind
  log "Maybe.orErr" (List.map (Maybe.orErr "Bad") [Just 1, Nothing, Just 2, Nothing, Just 3])
  testListCollapse
  Script.printLine "Unicode output test: ✅❌🙂"

main :: IO ()
main = Script.run script
