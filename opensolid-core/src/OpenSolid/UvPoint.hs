module OpenSolid.UvPoint
  ( UvPoint
  , data UvPoint
  , origin
  , coordinates
  , uCoordinate
  , vCoordinate
  , interiorSamples
  , leftSamples
  , rightSamples
  , bottomSamples
  , topSamples
  , random
  )
where

import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

-- | A point in UV parameter space.
type UvPoint = Point2D Unitless

{-# COMPLETE UvPoint #-}

-- | Construct a point from its U and V coordinates.
{-# INLINE UvPoint #-}
pattern UvPoint :: Number -> Number -> UvPoint
pattern UvPoint u v = Point2D u v

-- | The point (0, 0) in UV space.
origin :: UvPoint
origin = Point2D.origin

-- | Get the U and V coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: UvPoint -> (Number, Number)
coordinates = Point2D.coordinates

-- | Get the U coordinate of a point.
{-# INLINE uCoordinate #-}
uCoordinate :: UvPoint -> Number
uCoordinate = Point2D.xCoordinate

-- | Get the V coordinate of a point.
{-# INLINE vCoordinate #-}
vCoordinate :: UvPoint -> Number
vCoordinate = Point2D.yCoordinate

interiorSamples :: NonEmpty UvPoint
interiorSamples = do
  let (t1, t2, t3, t4, t5) = Quadrature.abscissae5
  let p1 = UvPoint t3 t3
  let p2 = UvPoint t2 t1
  let p3 = UvPoint t5 t2
  let p4 = UvPoint t4 t5
  let p5 = UvPoint t1 t4
  NonEmpty.five p1 p2 p3 p4 p5

boundarySamples :: (Number -> UvPoint) -> NonEmpty UvPoint
boundarySamples toPoint = NonEmpty.map toPoint Parameter.samples

leftSamples :: NonEmpty UvPoint
leftSamples = boundarySamples (\v -> UvPoint 0.0 v)

rightSamples :: NonEmpty UvPoint
rightSamples = boundarySamples (\v -> UvPoint 1.0 v)

bottomSamples :: NonEmpty UvPoint
bottomSamples = boundarySamples (\u -> UvPoint u 0.0)

topSamples :: NonEmpty UvPoint
topSamples = boundarySamples (\u -> UvPoint u 1.0)

random :: Random.Generator UvPoint
random = Random.map2 UvPoint Parameter.random Parameter.random
