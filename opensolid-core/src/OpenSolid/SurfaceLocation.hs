module OpenSolid.SurfaceLocation (SurfaceLocation (Point, Pole), bounds) where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint)

data SurfaceLocation
  = Point UvPoint
  | Pole (Nondegenerate UvCurve)
  deriving (Show)

instance Bounded SurfaceLocation UvBounds where
  bounds = bounds

bounds :: SurfaceLocation -> UvBounds
bounds surfaceLocation = case surfaceLocation of
  Point uvPoint -> UvBounds.constant uvPoint
  Pole (Nondegenerate uvCurve) -> do
    let curveBounds = UvCurve.bounds uvCurve
    assert (isOnBoundary curveBounds) curveBounds

isOnBoundary :: UvBounds -> Bool
isOnBoundary (UvBounds u v) =
  u == zero || u == one || v == zero || v == one

zero :: Interval Unitless
zero = Interval.constant 0.0

one :: Interval Unitless
one = Interval.constant 1.0
