module OpenSolid.UvPoint
  ( UvPoint
  , pattern UvPoint
  , origin
  , coordinates
  , uCoordinate
  , vCoordinate
  , distanceFrom
  , midpoint
  , interpolateFrom
  , samples
  , random
  )
where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

-- | A point in UV parameter space.
type UvPoint = Point2d Unitless UvSpace

{-# COMPLETE UvPoint #-}

-- | Construct a point from its U and V coordinates.
{-# INLINE UvPoint #-}
pattern UvPoint :: Number -> Number -> UvPoint
pattern UvPoint u v = Point2d u v

-- | The UV point with coordinates (0, 0).
origin :: UvPoint
origin = Point2d.origin

-- | Get the U and V coordinates of a point as a tuple.
coordinates :: UvPoint -> (Number, Number)
coordinates = Point2d.coordinates

-- | Get the U coordinate of a point.
uCoordinate :: UvPoint -> Number
uCoordinate = Point2d.xCoordinate

-- | Get the V coordinate of a point.
vCoordinate :: UvPoint -> Number
vCoordinate = Point2d.yCoordinate

-- | Compute the distance in UV space from one point to another.
distanceFrom :: UvPoint -> UvPoint -> Number
distanceFrom = Point2d.distanceFrom

-- | Find the midpoint between two points.
midpoint :: UvPoint -> UvPoint -> UvPoint
midpoint = Point2d.midpoint

interpolateFrom :: UvPoint -> UvPoint -> Number -> UvPoint
interpolateFrom = Point2d.interpolateFrom

samples :: List UvPoint
samples = do
  let (t1, t2, t3, t4, t5) = Quadrature.abscissae5
  let p1 = UvPoint t3 t3
  let p2 = UvPoint t2 t1
  let p3 = UvPoint t5 t2
  let p4 = UvPoint t4 t5
  let p5 = UvPoint t1 t4
  [p1, p2, p3, p4, p5]

random :: Random.Generator UvPoint
random = Random.map2 UvPoint Parameter.random Parameter.random
