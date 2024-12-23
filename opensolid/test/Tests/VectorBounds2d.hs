module Tests.VectorBounds2d (tests) where

import OpenSolid
import Parameter qualified
import Test (Test)
import Test qualified
import Tests.Random qualified as Random
import Vector2d qualified
import VectorBounds2d qualified

tests :: List Test
tests =
  [ placeIn
  , relativeTo
  ]

placeIn :: Test
placeIn = Test.check 100 "placeIn" Test.do
  localBounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let localVector = VectorBounds2d.interpolate localBounds u v
  frame <- Random.frame2d
  let globalBounds = VectorBounds2d.placeIn frame localBounds
  let globalVector = Vector2d.placeIn frame localVector
  Test.expect (VectorBounds2d.includes globalVector globalBounds)

relativeTo :: Test
relativeTo = Test.check 100 "relativeTo" Test.do
  globalBounds <- Random.vectorBounds2d
  u <- Parameter.random
  v <- Parameter.random
  let globalVector = VectorBounds2d.interpolate globalBounds u v
  frame <- Random.frame2d
  let localBounds = VectorBounds2d.relativeTo frame globalBounds
  let localVector = Vector2d.relativeTo frame globalVector
  Test.expect (VectorBounds2d.includes localVector localBounds)
