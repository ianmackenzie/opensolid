module Tests.Bounds2d (tests) where

import Bounds2d qualified
import OpenSolid
import Point2d qualified
import T qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ placeIn
  , relativeTo
  ]

placeIn :: Test
placeIn =
  Test.check 100 "placeIn" <| Test.do
    localBounds <- Random.bounds2d
    u <- T.generator
    v <- T.generator
    let localPoint = Bounds2d.interpolate localBounds u v
    frame <- Random.frame2d
    let globalBounds = Bounds2d.placeIn frame localBounds
    let globalPoint = Point2d.placeIn frame localPoint
    Test.expect (Bounds2d.includes globalPoint globalBounds)

relativeTo :: Test
relativeTo =
  Test.check 100 "relativeTo" <| Test.do
    globalBounds <- Random.bounds2d
    u <- T.generator
    v <- T.generator
    let globalPoint = Bounds2d.interpolate globalBounds u v
    frame <- Random.frame2d
    let localBounds = Bounds2d.relativeTo frame globalBounds
    let localPoint = Point2d.relativeTo frame globalPoint
    Test.expect (Bounds2d.includes localPoint localBounds)
