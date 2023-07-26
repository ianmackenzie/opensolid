module Tests.Qty (tests) where

import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Random qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ smallestBy
  , largestBy
  ]

smallestBy :: Test
smallestBy = Test.check 100 "smallestBy" $ do
  points <- Random.nonEmpty 10 Random.point2d
  let smallest = Qty.smallestBy Point2d.xCoordinate points
  let xMagnitude point = Qty.abs (Point2d.xCoordinate point)
  Test.expect (points |> NonEmpty.all (xMagnitude >> (>= xMagnitude smallest))) []

largestBy :: Test
largestBy = Test.check 100 "largestBy" $ do
  points <- Random.nonEmpty 10 Random.point2d
  let largest = Qty.largestBy Point2d.xCoordinate points
  let xMagnitude point = Qty.abs (Point2d.xCoordinate point)
  Test.expect (points |> NonEmpty.all (xMagnitude >> (<= xMagnitude largest))) []
