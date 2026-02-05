module OpenSolid.UvPoint
  ( UvPoint
  , pattern UvPoint
  , samples
  , random
  )
where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random
import OpenSolid.UvSpace (UvSpace)

-- | A point in UV parameter space.
type UvPoint = Point2D Unitless UvSpace

{-# COMPLETE UvPoint #-}

-- | Construct a point from its U and V coordinates.
{-# INLINE UvPoint #-}
pattern UvPoint :: Number -> Number -> UvPoint
pattern UvPoint u v = Point2D u v

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
