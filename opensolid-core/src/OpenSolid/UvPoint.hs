module OpenSolid.UvPoint (UvPoint, samples, random) where

import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

type UvPoint = Point2d UvSpace Unitless

samples :: List UvPoint
samples = do
  let (t1, t2, t3, t4, t5) = Quadrature.abscissae5
  let p1 = Point2d t3 t3
  let p2 = Point2d t2 t1
  let p3 = Point2d t5 t2
  let p4 = Point2d t4 t5
  let p5 = Point2d t1 t4
  [p1, p2, p3, p4, p5]

random :: Random.Generator UvPoint
random = Random.map2 Point2d Parameter.random Parameter.random
