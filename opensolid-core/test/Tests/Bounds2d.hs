module Tests.Bounds2d (tests) where

import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import Test (Test)
import Test qualified
import Tests.Random qualified as Random

tests :: Tolerance Meters => List Test
tests =
  [ placeIn
  , relativeTo
  , transformBy
  ]

boundsAndContainedPoint :: Generator (Bounds2d (space @ Meters), Point2d (space @ Meters))
boundsAndContainedPoint = Random.do
  bounds <- Random.bounds2d
  u <- Parameter.random
  v <- Parameter.random
  let point = Bounds2d.interpolate bounds u v
  Random.return (bounds, point)

placeIn :: Tolerance Meters => Test
placeIn = Test.check 100 "placeIn" Test.do
  (localBounds, localPoint) <- boundsAndContainedPoint
  frame <- Random.frame2d
  let globalBounds = Bounds2d.placeIn frame localBounds
  let globalPoint = Point2d.placeIn frame localPoint
  Test.expect (globalPoint ^ globalBounds)

relativeTo :: Tolerance Meters => Test
relativeTo = Test.check 100 "relativeTo" Test.do
  (globalBounds, globalPoint) <- boundsAndContainedPoint
  frame <- Random.frame2d
  let localBounds = Bounds2d.relativeTo frame globalBounds
  let localPoint = Point2d.relativeTo frame globalPoint
  Test.expect (localPoint ^ localBounds)

transformBy :: Tolerance Meters => Test
transformBy = Test.check 100 "transformBy" Test.do
  (originalBounds, originalPoint) <- boundsAndContainedPoint
  transform <- Random.affineTransform2d
  let transformedBounds = Bounds2d.transformBy transform originalBounds
  let transformedPoint = Point2d.transformBy transform originalPoint
  Test.expect (transformedPoint ^ transformedBounds)
