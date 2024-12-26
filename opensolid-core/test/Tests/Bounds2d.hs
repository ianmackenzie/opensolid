module Tests.Bounds2d (tests) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: List Test
tests =
  [ placeIn
  , relativeTo
  ]

placeIn :: Test
placeIn = Test.check 100 "placeIn" Test.do
  localBounds <- Random.bounds2d
  u <- Parameter.random
  v <- Parameter.random
  let localPoint = Bounds2d.interpolate localBounds u v
  frame <- Random.frame2d
  let globalBounds = Bounds2d.placeIn frame localBounds
  let globalPoint = Point2d.placeIn frame localPoint
  Test.expect (Bounds2d.includes globalPoint globalBounds)

relativeTo :: Test
relativeTo = Test.check 100 "relativeTo" Test.do
  globalBounds <- Random.bounds2d
  u <- Parameter.random
  v <- Parameter.random
  let globalPoint = Bounds2d.interpolate globalBounds u v
  frame <- Random.frame2d
  let localBounds = Bounds2d.relativeTo frame globalBounds
  let localPoint = Point2d.relativeTo frame globalPoint
  Test.expect (Bounds2d.includes localPoint localBounds)
