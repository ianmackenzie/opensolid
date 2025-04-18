module OpenSolid.SurfaceParameter
  ( SurfaceParameter (U, V)
  , UvSpace
  , UvCoordinates
  , UvPoint
  , UvDirection
  , UvBounds
  , domain
  , random
  , samples
  )
where

import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Range qualified as Range

data SurfaceParameter = U | V deriving (Eq, Show, Ord)

data UvSpace deriving (Eq, Show)

type UvCoordinates = UvSpace @ Unitless

type UvPoint = Point2d UvCoordinates

type UvBounds = Bounds2d UvCoordinates

type UvDirection = Direction2d UvSpace

domain :: UvBounds
domain = Bounds2d Range.unit Range.unit

random :: Generator UvPoint
random = Random.map2 Point2d Parameter.random Parameter.random

samples :: List UvPoint
samples = do
  let (t1, t2, t3, t4) = Quadrature.abscissae4
  [Point2d.xy t1 t3, Point2d.xy t2 t1, Point2d.xy t3 t4, Point2d.xy t4 t2]
